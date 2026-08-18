// ═══════════════════════════════════════════════
// GLOBAL STATE
// ═══════════════════════════════════════════════
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
let isGiveUpMode = false;
let currentTheme = 'dark';

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