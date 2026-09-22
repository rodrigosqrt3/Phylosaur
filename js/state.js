// ═══════════════════════════════════════════════
// GLOBAL STATE
// ═══════════════════════════════════════════════
const PHYLOSAUR_RELEASE_VERSION = 42;
const PHYLOSAUR_STORAGE_KEYS = Object.freeze({
  theme: 'phylosaur-theme',
  visitorId: 'phylosaur-visitor-id',
  guestAchievements: 'phylosaur-guest-achievements-v1',
  tutorialComplete: 'phylosaur-tutorial-v1-complete',
  museumImageCache: 'phylosaur-image-cache-v5',
  legacyDiscoveries: 'phylosaur-discoveries',
  discoveryEvents: 'phylosaur-discovery-events-v1',
  accountDiscoveryPrefix: 'phylosaur-account-discoveries-v1:'
});

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
let challengeRaceClosing = false;
let isGiveUpMode = false;
let currentTheme = 'dark';
let gameSessionId = null;
let currentTargetDepth = 0;
let serverPossibleSpecimens = 0;
let gameRequestPending = false;
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