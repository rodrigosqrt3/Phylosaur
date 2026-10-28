// ═══════════════════════════════════════════════
// GLOBAL STATE
// ═══════════════════════════════════════════════
const PHYLOSAUR_RELEASE_VERSION = 241;
const PHYLOSAUR_STORAGE_KEYS = Object.freeze({
  theme: 'phylosaur-theme',
  language: 'phylosaur-language',
  visitorId: 'phylosaur-visitor-id',
  guestAchievements: 'phylosaur-guest-achievements-v1',
  tutorialComplete: 'phylosaur-tutorial-v1-complete',
  museumImageCache: 'phylosaur-image-cache-v5',
  legacyDiscoveries: 'phylosaur-discoveries',
  discoveryEvents: 'phylosaur-discovery-events-v1',
  accountDiscoveryPrefix: 'phylosaur-account-discoveries-v1:',
  sessionCredentialPrefix: 'phylosaur-session-credential-v1:',
  reloadScroll: 'phylosaur-reload-scroll-v1',
  customChallenge: 'phylosaur-custom-challenge-v1'
});

// Custom challenges: a pool key such as 'custom:muito_facil,facil:Theropoda' travels
// wherever a difficulty does (mirrors parsePoolKey in the edge function). One level
// without a clade is just that level, so classic challenges keep working.
const PHYLOSAUR_LEVELS = Object.freeze(['muito_facil', 'facil', 'normal', 'dificil', 'muito_dificil']);
const CUSTOM_POOL_CLADES = Object.freeze(['Theropoda', 'Sauropodomorpha', 'Ornithischia']);
const MINIMUM_CUSTOM_POOL_SIZE = 10;

function buildPoolKey(levels, clade) {
  const chosen = PHYLOSAUR_LEVELS.filter(level => levels.includes(level));
  const validClade = CUSTOM_POOL_CLADES.includes(clade) ? clade : null;
  if (chosen.length === 1 && !validClade) return chosen[0];
  return chosen.length ? `custom:${chosen.join(',')}:${validClade || '*'}` : '';
}

function parseCustomPoolKey(value) {
  const match = /^custom:([a-z_,]+):([A-Za-z]+|\*)$/.exec(String(value || ''));
  if (!match) return null;
  const levels = match[1].split(',');
  const clade = match[2] === '*' ? null : match[2];
  if (levels.some(level => !PHYLOSAUR_LEVELS.includes(level)) || new Set(levels).size !== levels.length) return null;
  if (clade && !CUSTOM_POOL_CLADES.includes(clade)) return null;
  return { levels, clade };
}

// Practice "play again": a custom pool starts another game with the same filters.
function getPracticeReplayAction() {
  return parseCustomPoolKey(selectedDifficulty)
    ? `startPracticeChallenge('${selectedDifficulty}')`
    : 'showPracticeMode()';
}

// "Levels I, II · Theropoda" style label for a custom pool.
function describeCustomPool(value) {
  const pool = parseCustomPoolKey(value);
  if (!pool) return '';
  const numerals = pool.levels.map(level => ['I', 'II', 'III', 'IV', 'V'][PHYLOSAUR_LEVELS.indexOf(level)]);
  return [`${t('friends.level')} ${numerals.join(', ')}`, pool.clade].filter(Boolean).join(' · ');
}

function escapeHtml(value) {
  return String(value ?? '').replace(/[&<>'"]/g, character => ({
    '&': '&amp;', '<': '&lt;', '>': '&gt;', "'": '&#39;', '"': '&quot;'
  })[character]);
}

let fullDatabase = [];
let database = [];
let targetDino = null;
let guesses = [];
let hintsRemaining = 3;
let gameWon = false;
let guessedNames = new Set();
let revealedClades = new Set();
let selectedDifficulty = null;
let hintHistory = [];
let guessesSinceLastHint = 0;
let isPracticeMode = false;
let currentGameMode = 'daily';
let currentChallengeCode = null;
let currentChallengePlayerName = null;
let currentChallengeCreatorName = null;
let currentChallengePlacement = null;
let currentChallengeTotalPlayers = 0;
let currentChallengeEliminated = false;
let challengeStatusPollTimer = null;
let challengeStatusPollInFlight = false;
let challengeStatusPollGeneration = 0;
let challengeRaceClosing = false;
let isGiveUpMode = false;
let currentTheme = 'dark';
let gameSessionId = null;
let currentTargetDepth = 0;
let serverPossibleSpecimens = 0;
let gameRequestPending = false;
let gameActionGeneration = 0;
let currentMuseumProof = null;
let currentAccountProgress = null;
let isAnalyticsAdmin = false;
let analyticsAccessChecked = false;

// ═══════════════════════════════════════════════
// USER STATE
// ═══════════════════════════════════════════════
let currentUser = null;
let currentUserId = null;
let userStats = {
  gamesPlayed: 0,
  gamesWon: 0,
  totalGuesses: 0,
  bestScore: null,
  difficultyStats: {
    'muito_facil':   { played: 0, won: 0, avgGuesses: 0 },
    'facil':         { played: 0, won: 0, avgGuesses: 0 },
    'normal':        { played: 0, won: 0, avgGuesses: 0 },
    'dificil':       { played: 0, won: 0, avgGuesses: 0 },
    'muito_dificil': { played: 0, won: 0, avgGuesses: 0 }
  },
  recentGames: [],
  achievements: []
};