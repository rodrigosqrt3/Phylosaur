// ═══════════════════════════════════════════════════════════════════════
// DATABSE OPERATIONS
// ═══════════════════════════════════════════════════════════════════════
let dailyCompletionCache = null;

const ACHIEVEMENT_DEFINITIONS = [
    { id: 'first_win', name: 'First Win', desc: 'Complete your first challenge' },
    { id: 'perfect_game', name: 'Three Guesses', desc: 'Find the answer in 3 guesses or fewer' },
    { id: 'ten_wins', name: '10 Wins', desc: 'Complete 10 challenges' },
    { id: 'fifty_wins', name: '50 Wins', desc: 'Complete 50 challenges' },
    { id: 'hard_win', name: 'Level IV', desc: 'Complete a Level IV challenge' },
    { id: 'very_hard_win', name: 'Level V', desc: 'Complete a Level V challenge' },
    { id: 'field_researcher', name: 'Field Researcher', desc: 'Complete 10 Daily challenges' },
    { id: 'persistence_pays', name: 'Persistence Pays', desc: 'Win a Daily challenge after 10 or more attempts' },
    { id: 'independent_thinker', name: 'Independent Thinker', desc: 'Win a Daily challenge without using a hint' },
    { id: 'complete_classification', name: 'Complete Classification', desc: 'Win a Daily challenge on every level' },
    { id: 'three_day_expedition', name: 'Three-Day Expedition', desc: 'Reach a 3-day streak' },
    { id: 'seven_day_expedition', name: 'Seven-Day Expedition', desc: 'Reach a 7-day streak' }
];

const GUEST_ACHIEVEMENT_PROGRESS_KEY = 'phylosaur-guest-achievements-v1';

function readGuestAchievementProgress() {
    try {
        const stored = JSON.parse(localStorage.getItem(GUEST_ACHIEVEMENT_PROGRESS_KEY) || '{}');
        return {
            results: stored.results && typeof stored.results === 'object' ? stored.results : {},
            unlocked: Array.isArray(stored.unlocked) ? stored.unlocked : []
        };
    } catch (error) {
        console.warn('Could not read guest achievement progress:', error);
        return { results: {}, unlocked: [] };
    }
}

function writeGuestAchievementProgress(progress) {
    try {
        localStorage.setItem(GUEST_ACHIEVEMENT_PROGRESS_KEY, JSON.stringify(progress));
    } catch (error) {
        console.warn('Could not save guest achievement progress:', error);
    }
}

function getGuestAchievementIds() {
    return readGuestAchievementProgress().unlocked.filter(id =>
        ACHIEVEMENT_DEFINITIONS.some(definition => definition.id === id)
    );
}

function getLongestGuestDailyStreak(results) {
    const winningDates = [...new Set(results
        .filter(result => result.won && /^\d{4}-\d{2}-\d{2}$/.test(result.playedDate))
        .map(result => result.playedDate))]
        .sort();

    let longest = 0;
    let current = 0;
    let previousTime = null;

    winningDates.forEach(dateString => {
        const time = new Date(`${dateString}T00:00:00Z`).getTime();
        current = previousTime !== null && time - previousTime === 86400000
            ? current + 1
            : 1;
        longest = Math.max(longest, current);
        previousTime = time;
    });

    return longest;
}

function evaluateGuestAchievements(results) {
    const wins = results.filter(result => result.won);
    const wonLevels = new Set(wins.map(result => result.difficulty));
    const longestStreak = getLongestGuestDailyStreak(results);

    return [
        ['first_win', wins.length >= 1],
        ['perfect_game', wins.some(result => result.guessCount <= 3)],
        ['ten_wins', wins.length >= 10],
        ['fifty_wins', wins.length >= 50],
        ['hard_win', wins.some(result => result.difficulty === 'dificil')],
        ['very_hard_win', wins.some(result => result.difficulty === 'muito_dificil')],
        ['field_researcher', results.length >= 10],
        ['persistence_pays', wins.some(result => result.guessCount >= 10)],
        ['independent_thinker', wins.some(result => !result.usedHints)],
        ['complete_classification', ['muito_facil', 'facil', 'normal', 'dificil', 'muito_dificil']
            .every(difficulty => wonLevels.has(difficulty))],
        ['three_day_expedition', longestStreak >= 3],
        ['seven_day_expedition', longestStreak >= 7]
    ].filter(([, complete]) => complete).map(([id]) => id);
}

function recordGuestDailyResult(won) {
    if (currentGameMode !== 'daily' || currentUserId) return [];

    const progress = readGuestAchievementProgress();
    const resultKey = gameSessionId || [
        getTodayString(),
        selectedDifficulty,
        targetDino?.nome || 'unknown'
    ].join(':');

    if (!progress.results[resultKey]) {
        progress.results[resultKey] = {
            sessionId: gameSessionId || null,
            playedDate: getTodayString(),
            difficulty: selectedDifficulty,
            won: won === true,
            guessCount: guesses.length,
            usedHints: hintHistory.length > 0,
            completedAt: new Date().toISOString()
        };
    }

    const results = Object.values(progress.results)
        .sort((first, second) => String(first.completedAt).localeCompare(String(second.completedAt)))
        .slice(-500);
    progress.results = Object.fromEntries(results.map(result => [
        result.sessionId || [result.playedDate, result.difficulty, result.completedAt].join(':'),
        result
    ]));

    const unlocked = new Set(progress.unlocked);
    const newlyUnlocked = evaluateGuestAchievements(results)
        .filter(id => !unlocked.has(id));
    newlyUnlocked.forEach(id => unlocked.add(id));
    progress.unlocked = [...unlocked];
    writeGuestAchievementProgress(progress);

    newlyUnlocked.forEach(showAchievementNotification);
    return newlyUnlocked;
}

async function updateStatsAfterGame(won, guessCount, difficulty) {
  if (!currentUserId) return [];

    const { data: current } = await sb.from('statistics')
        .select('*')
        .eq('user_id', currentUserId)
        .single();

    const base = current || {
        games_played: 0,
        games_won: 0,
        total_guesses: 0,
        best_score: null
    };

    const newPlayed  = base.games_played + 1;
    const newWon     = base.games_won + (won ? 1 : 0);
    const newTotal   = base.total_guesses + guessCount;
    const newBest    = won && (base.best_score === null || guessCount < base.best_score)
                        ? guessCount
                        : base.best_score;

    await sb.from('statistics').upsert({
        user_id:      currentUserId,
        games_played: newPlayed,
        games_won:    newWon,
        total_guesses: newTotal,
        best_score:   newBest,
        updated_at:   new Date().toISOString()
    }, { onConflict: 'user_id' });

    userStats.gamesPlayed  = newPlayed;
    userStats.gamesWon     = newWon;
    userStats.totalGuesses = newTotal;
    userStats.bestScore    = newBest;

    try {
        return await checkAchievements(won, guessCount, {
            usedHints: hintHistory.length > 0,
            currentStreak: Number(base.current_streak || 0),
            bestStreak: Number(base.best_streak || 0),
            difficulty
        });
    } catch (error) {
        console.error('Achievement check failed:', error);
        return [];
    }
    }

async function checkAchievements(won, guessCount, context = {}) {
    if (!currentUserId) return;

    const { data: completedLevels } = await sb.from('daily_results')
        .select('difficulty')
        .eq('user_id', currentUserId)
        .eq('won', true);

    const gameDifficulty = context.difficulty || selectedDifficulty;
    const wonLevels = new Set((completedLevels || []).map(result => result.difficulty));
    if (won && gameDifficulty) wonLevels.add(gameDifficulty);

    const allLevels = ['muito_facil', 'facil', 'normal', 'dificil', 'muito_dificil'];
    const completedEveryLevel = allLevels.every(difficulty => wonLevels.has(difficulty));
    const currentStreak = Math.max(
        Number(context.currentStreak || 0),
        Number(context.bestStreak || 0)
    );
    const usedHints = context.usedHints === true;
    
    const achievements = [
        { id: 'first_win', condition: () => userStats.gamesWon >= 1 },
        { id: 'perfect_game', condition: () => won && guessCount <= 3 },
        { id: 'ten_wins', condition: () => userStats.gamesWon >= 10 },
        { id: 'fifty_wins', condition: () => userStats.gamesWon >= 50 },
        { id: 'hard_win', condition: () => won && gameDifficulty === 'dificil' },
        { id: 'very_hard_win', condition: () => won && gameDifficulty === 'muito_dificil' },
        { id: 'field_researcher', condition: () => userStats.gamesPlayed >= 10 },
        { id: 'persistence_pays', condition: () => won && guessCount >= 10 },
        { id: 'independent_thinker', condition: () => won && !usedHints },
        { id: 'complete_classification', condition: () => completedEveryLevel },
        { id: 'three_day_expedition', condition: () => currentStreak >= 3 },
        { id: 'seven_day_expedition', condition: () => currentStreak >= 7 }
    ];

    const { data: existing } = await sb.from('achievements')
        .select('achievement_id')
        .eq('user_id', currentUserId);

    const unlocked = new Set(existing ? existing.map(a => a.achievement_id) : []);
    const newlyUnlocked = [];

    for (const ach of achievements) {
        if (ach.condition() && !unlocked.has(ach.id)) {
        const { error } = await sb.from('achievements')
            .insert({ user_id: currentUserId, achievement_id: ach.id });
        if (error) {
            console.error(`Could not unlock achievement ${ach.id}:`, error);
            continue;
        }
        unlocked.add(ach.id);
        newlyUnlocked.push(ach.id);
        showAchievementNotification(ach.id);
        }
    }

    return newlyUnlocked;
}

function buildAchievementProgress(stats = {}, wonResults = []) {
    const gamesPlayed = Number(stats?.games_played || 0);
    const gamesWon = Number(stats?.games_won || 0);
    const bestStreak = Math.max(
        Number(stats?.current_streak || 0),
        Number(stats?.best_streak || 0)
    );
    const wins = Array.isArray(wonResults) ? wonResults.filter(result => result?.won !== false) : [];
    const wonLevels = new Set(wins.map(result => result.difficulty).filter(Boolean));
    const requiredLevels = ['muito_facil', 'facil', 'normal', 'dificil', 'muito_dificil'];
    const completedLevelCount = requiredLevels.filter(difficulty => wonLevels.has(difficulty)).length;
    const hasPerfectGame = wins.some(result => {
        const guesses = Number(result.guess_count);
        return Number.isFinite(guesses) && guesses > 0 && guesses <= 3;
    });
    const hasPersistenceWin = wins.some(result => {
        const guesses = Number(result.guess_count);
        return Number.isFinite(guesses) && guesses >= 10;
    });
    const hasHintlessWin = wins.some(result =>
        Array.isArray(result.hint_history) && result.hint_history.length === 0
    );

    const numeric = (current, target, unit) => ({
        current: Math.min(Number(current || 0), target),
        target,
        unit,
        complete: Number(current || 0) >= target
    });
    const binary = complete => ({
        current: complete ? 1 : 0,
        target: 1,
        unit: '',
        complete
    });

    return {
        first_win: numeric(gamesWon, 1, 'win'),
        perfect_game: binary(hasPerfectGame),
        ten_wins: numeric(gamesWon, 10, 'wins'),
        fifty_wins: numeric(gamesWon, 50, 'wins'),
        hard_win: binary(wonLevels.has('dificil')),
        very_hard_win: binary(wonLevels.has('muito_dificil')),
        field_researcher: numeric(gamesPlayed, 10, 'games'),
        persistence_pays: binary(hasPersistenceWin),
        independent_thinker: binary(hasHintlessWin),
        complete_classification: numeric(completedLevelCount, 5, 'levels'),
        three_day_expedition: numeric(bestStreak, 3, 'days'),
        seven_day_expedition: numeric(bestStreak, 7, 'days')
    };
}

async function syncHistoricalAchievements(stats, wonResults, unlockedSet = new Set()) {
    if (!currentUserId) {
        return { unlockedSet, newlyUnlocked: [], progress: buildAchievementProgress(stats, wonResults) };
    }

    const progress = buildAchievementProgress(stats, wonResults);
    const newlyUnlocked = [];

    for (const definition of ACHIEVEMENT_DEFINITIONS) {
        if (!progress[definition.id]?.complete || unlockedSet.has(definition.id)) continue;

        const { error } = await sb.from('achievements')
            .insert({ user_id: currentUserId, achievement_id: definition.id });
        if (error) {
            console.error(`Could not synchronize achievement ${definition.id}:`, error);
            continue;
        }

        unlockedSet.add(definition.id);
        newlyUnlocked.push(definition.id);
        showAchievementNotification(definition.id);
    }

    return { unlockedSet, newlyUnlocked, progress };
}

function showAchievementNotification(name) {
    const definition = ACHIEVEMENT_DEFINITIONS.find(achievement => achievement.id === name);
    const displayName = definition?.name || name;
    let stack = document.getElementById('achievement-notification-stack');
    if (!stack) {
        stack = document.createElement('div');
        stack.id = 'achievement-notification-stack';
        stack.className = 'achievement-notification-stack';
        stack.setAttribute('aria-live', 'polite');
        stack.setAttribute('aria-label', 'Achievement notifications');
        document.body.appendChild(stack);
    }

    const notif = document.createElement('div');
    notif.className = 'achievement-notification';
      notif.innerHTML = `
        <div class="achievement-notification-kicker">Achievement Unlocked!</div>
        <div class="achievement-notification-title">${displayName}</div>
      `;
      stack.appendChild(notif);
      
      setTimeout(() => {
        notif.classList.add('is-leaving');
        setTimeout(() => {
            notif.remove();
            if (!stack.children.length) stack.remove();
        }, 350);
      }, 3000);
}

async function clearGameProgress(difficulty) {
    if (!currentUserId) return;
    const today = getTodayString();
    await sb.from('daily_results')
    .delete()
    .eq('user_id', currentUserId)
    .eq('played_date', today)
    .eq('difficulty', difficulty)
    .eq('won', false); 
}

async function markDailyChallengeCompleted(difficulty) {
    if (!currentUserId) return;
    const today = getTodayString();
    await sb.from('daily_results').upsert({
    user_id: currentUserId,
    played_date: today,
    difficulty: difficulty,
    target_dino: targetDino.nome,
    guess_count: guesses.length,
    won: gameWon,
    guesses: guesses.map(g => ({ nome: g.dino.nome, isHint: g.isHint || false })),
    revealed_clades: Array.from(revealedClades),
    hint_history: hintHistory
    }, { onConflict: 'user_id,played_date,difficulty' });
    dailyCompletionCache = null;
}

async function getStreakData() {
    if (!currentUserId) return { current: 0, best: 0, lastPlayed: null };

    const { data } = await sb.from('statistics')
    .select('current_streak, best_streak, last_played')
    .eq('user_id', currentUserId)
    .single();

    if (!data) return { current: 0, best: 0, lastPlayed: null };
    return { current: data.current_streak || 0, best: data.best_streak || 0, lastPlayed: data.last_played };
}

async function saveStreakData(streakData) {
    if (!currentUserId) return;

    await sb.from('statistics').upsert({
    user_id: currentUserId,
    current_streak: streakData.current,
    best_streak: streakData.best,
    last_played: streakData.lastPlayed
    }, { onConflict: 'user_id' });
}

async function updateStreak() {
    if (!currentUserId) return { current: 0, best: 0 };

    const today = getTodayString();
    const streakData = await getStreakData();

    if (streakData.lastPlayed === today) return streakData;

    const yesterdayStr = getUtcDateOffsetString(-1);
    const tomorrowStr = getUtcDateOffsetString(1);

    // Compatibility with streaks saved by the old local-date system in
    // time zones that were already on the following calendar day.
    if (streakData.lastPlayed === tomorrowStr) {
        streakData.lastPlayed = today;
        await saveStreakData(streakData);
        return streakData;
    }

    if (streakData.lastPlayed === yesterdayStr) {
    streakData.current++;
    } else if (!streakData.lastPlayed) {
    streakData.current = 1;
    } else {
    streakData.current = 1;
    }

    if (streakData.current > streakData.best) streakData.best = streakData.current;
    streakData.lastPlayed = today;

    await saveStreakData(streakData);
    return streakData;
}

function checkStreakMilestone(streak) {
    const milestones = [3, 7, 14, 30, 50, 100];
    
    for (let milestone of milestones) {
    if (streak === milestone) {
        return milestone;
    }
    }
    
    return null;
}

async function getDailyCompletionStatus() {
    if (!currentUserId) return { muito_facil: false, facil: false, normal: false, dificil: false, muito_dificil: false };
    
    const today = getTodayString();
    const cacheKey = `${currentUserId}:${today}`;
    if (dailyCompletionCache?.key === cacheKey) {
        return { ...dailyCompletionCache.status };
    }
    const { data } = await sb.from('daily_results')
    .select('difficulty')
    .eq('user_id', currentUserId)
    .eq('played_date', today)
    .eq('won', true);
    
    const status = { muito_facil: false, facil: false, normal: false, dificil: false, muito_dificil: false };
    if (data) data.forEach(row => { status[row.difficulty] = true; });
    dailyCompletionCache = { key: cacheKey, status: { ...status } };
    return status;
}  

function getTodayString() {
    return new Date().toISOString().slice(0, 10);
}

function getUtcDateOffsetString(dayOffset) {
    const date = new Date();
    date.setUTCDate(date.getUTCDate() + dayOffset);
    return date.toISOString().slice(0, 10);
}

function applyServerGamePayload(data) {
    gameSessionId = data.sessionId || gameSessionId;
    if (data.mode) {
        currentGameMode = data.mode;
        isPracticeMode = data.mode === 'practice';
    }
    if (data.challenge) {
        currentChallengeCode = data.challenge.code || currentChallengeCode;
        currentChallengePlayerName = data.challenge.playerName || currentChallengePlayerName;
        currentChallengeCreatorName = data.challenge.creatorName || currentChallengeCreatorName;
        currentChallengePlacement = data.challenge.placement ?? currentChallengePlacement;
        currentChallengeTotalPlayers = Number(data.challenge.totalPlayers ?? currentChallengeTotalPlayers ?? 0);
        currentChallengeEliminated = Boolean(data.challenge.eliminated);
    }
    currentTargetDepth = Number(
        data.targetDepth || data.guess?.targetDepth || data.target?.profundidade || currentTargetDepth || 0
    );
    serverPossibleSpecimens = Number(data.possibleSpecimens ?? serverPossibleSpecimens ?? 0);

    if (Array.isArray(data.availableNames)) {
        database = data.availableNames.map(nome => ({ nome }));
    }

    if (Array.isArray(data.guesses)) {
        guesses = data.guesses.map(record => ({
            dino: { nome: record.nome },
            proximity: {
                matches: Number(record.matches || 0),
                percentage: Number(record.percentage || 0),
                lastCommonClade: record.lastCommonClade || null,
                divergenceDepth: Number(record.matches || 0)
            },
            isHint: record.isHint === true
        }));
    }

    if (Array.isArray(data.revealedClades)) {
        revealedClades = new Set(data.revealedClades);
    }
    if (Array.isArray(data.hintHistory)) hintHistory = data.hintHistory;
    if (data.hintsRemaining !== undefined) hintsRemaining = Number(data.hintsRemaining);
    if (data.guessesSinceHint !== undefined) guessesSinceLastHint = Number(data.guessesSinceHint);

    guessedNames = new Set(guesses.map(guess => guess.dino.nome.toLowerCase()));
    gameWon = Boolean(data.won || data.complete);
    isGiveUpMode = Boolean(data.gaveUp);
    if (data.museumProof) currentMuseumProof = data.museumProof;

    if (data.target) {
        targetDino = {
            nome: data.target.nome,
            linhagem: Array.isArray(data.target.linhagem) ? data.target.linhagem : [],
            profundidade: data.target.profundidade
        };
    } else {
        targetDino = null;
    }

    if (data.tree) window.currentTreeSnapshot = data.tree;
}

function setServerStatValue(element, value, animate) {
    if (!element) return;

    const nextValue = String(value);
    const changed = element.textContent !== nextValue;
    element.textContent = nextValue;

    if (animate && changed) {
        element.classList.remove('stat-value-updated');
        void element.getBoundingClientRect();
        element.classList.add('stat-value-updated');
    }
}

function updateHintButtonState() {
    const button = document.querySelector('.btn-game-hint');
    if (!button) return;

    const guessesRequired = Math.max(0, 2 - guessesSinceLastHint);
    const unavailable = gameWon || hintsRemaining <= 0 || guessesRequired > 0;
    button.disabled = unavailable;

    if (gameWon) {
        button.title = 'This game is complete';
    } else if (hintsRemaining <= 0) {
        button.title = 'No hints remaining';
    } else if (guessesRequired > 0) {
        button.title = `Make ${guessesRequired} more ${guessesRequired === 1 ? 'guess' : 'guesses'} before using a hint`;
    } else {
        button.title = 'Reveal the next clade in the hidden lineage';
    }

    button.textContent = guessesRequired > 0
        ? `Hint · ${guessesRequired} ${guessesRequired === 1 ? 'guess' : 'guesses'}`
        : 'Hint';
}

function updateServerGameDisplay(data) {
    const bestMatch = guesses.length > 0
        ? Math.max(...guesses.map(guess => guess.proximity.matches))
        : 0;

    const attempts = document.getElementById('attempts');
    const hints = document.getElementById('hints');
    const best = document.getElementById('best-match');
    const clades = document.getElementById('clades-revealed');
    const possible = document.getElementById('possible-specimens');
    const animationMode = typeof getTreeAnimationMode === 'function'
        ? getTreeAnimationMode()
        : 'default';
    const animateStats = animationMode === 'guess' || animationMode === 'hint';

    setServerStatValue(attempts, guesses.length, animateStats);
    setServerStatValue(hints, hintsRemaining, animateStats);
    setServerStatValue(best, bestMatch, animateStats);
    setServerStatValue(clades, revealedClades.size, animateStats);
    setServerStatValue(possible, serverPossibleSpecimens, animateStats);
    updateHintButtonState();

    const wrapper = document.getElementById('tree-scroll-wrapper');
    if (data.tree && (guesses.length > 0 || hintHistory.length > 0 || data.complete)) {
        renderTreeSnapshot(data.tree);
    } else if (wrapper) {
        wrapper.innerHTML = '<div class="empty-state">The tree will appear after your first guess or hint.</div>';
    }

    updateCladeInfo();
    updateGuessHistory();
}

async function showRestoredServerCompletion(data) {
    const input = document.getElementById('dino-input');
    if (input) input.disabled = true;
    document.querySelector('.btn-guess')?.setAttribute('disabled', true);
    document.querySelector('.btn-game-hint')?.setAttribute('disabled', true);
    document.querySelector('.btn-giveup')?.setAttribute('disabled', true);

    const container = document.getElementById('tree-container');
    if (!container || container.querySelector('.victory')) return;

    const targetName = data.target?.nome || targetDino?.nome || '';
    const resultMedia = targetName ? await loadResultMedia(targetName) : null;

    const panel = document.createElement('div');
    const wasRaceEliminated = currentGameMode === 'challenge' &&
        Boolean(data.challenge?.eliminated || currentChallengeEliminated);
    const resultWasRevealed = data.gaveUp || wasRaceEliminated;
    panel.className = `victory${resultWasRevealed ? ' victory--revealed' : ''}`;
    panel.innerHTML = `
        <div class="victory-heading">
            <h2>${wasRaceEliminated ? 'RACE COMPLETE' : data.gaveUp ? 'ANSWER REVEALED' : 'CHALLENGE COMPLETE'}</h2>
            <div class="victory-dino">${targetName}</div>
            <div class="victory-summary" aria-label="Game result summary">
                <span>${guesses.length} ${guesses.length === 1 ? 'attempt' : 'attempts'}</span>
                ${resultWasRevealed ? '<span>Answer revealed</span>' : ''}
            </div>
        </div>

        ${wasRaceEliminated ? `
        <div class="race-placement-card">
            <strong>#${currentChallengePlacement || currentChallengeTotalPlayers}</strong>
            <span>The other finishing positions were secured, so your race ended automatically.</span>
        </div>` : currentGameMode === 'challenge' && currentChallengePlacement ? `
        <div class="race-placement-card"><strong>#${currentChallengePlacement}</strong><span>Your current finishing position</span></div>` : ''}

        ${buildResultMediaMarkup(targetName, resultMedia)}

        <div class="victory-actions">
            <button class="btn-hint victory-action-secondary" onclick="toggleResultTreeView(true)">View Tree</button>
            ${currentGameMode === 'challenge' ? `
            <button class="btn-hint victory-action-secondary" onclick="showChallengeStandings()">View Standings</button>
            <button class="btn-new-game" onclick="showFriendChallenges()">Return to Friend Challenges</button>` : `
            <button class="btn-new-game" onclick="${isPracticeMode ? 'showPracticeMode()' : 'showDifficultySelection()'}">
                ${isPracticeMode ? 'Play Again' : 'Return to Level Selection'}
            </button>`}
        </div>
    `;
    container.insertBefore(panel, container.firstChild);
    bindResultMedia(panel, targetName, resultMedia);
    revealResultPanel(container, panel);
}

async function loadServerDatabase(mode, difficulty, forceClean = false, resumeAutomatically = false) {
    window.collapsedClades.clear();
    window.currentTreeSnapshot = null;
    if (typeof resetTreeAnimationState === 'function') resetTreeAnimationState();
    isPracticeMode = mode === 'practice';
    currentGameMode = mode;
    currentChallengeCode = null;
    currentChallengePlayerName = null;
    currentChallengeCreatorName = null;
    currentChallengePlacement = null;
    currentChallengeTotalPlayers = 0;
    currentChallengeEliminated = false;
    challengeRaceClosing = false;
    gameSessionId = null;
    targetDino = null;
    guesses = [];
    hintsRemaining = 3;
    gameWon = false;
    guessedNames = new Set();
    revealedClades = new Set();
    hintHistory = [];
    guessesSinceLastHint = 0;
    currentTargetDepth = 0;
    serverPossibleSpecimens = 0;
    gameRequestPending = false;
    currentMuseumProof = null;

    const wrapper = document.getElementById('tree-scroll-wrapper');
    if (wrapper) wrapper.innerHTML = '<div class="loading">Loading challenge...</div>';

    const storageKey = getGameSessionStorageKey(mode, difficulty);
    if (forceClean) localStorage.removeItem(storageKey);

    let data = null;
    const storedSessionId = forceClean ? null : localStorage.getItem(storageKey);

    if (storedSessionId) {
        try {
            data = await callGameApi('state', { sessionId: storedSessionId });

            const hasSavedProgress = !data.complete && Number(data.attempts || 0) > 0;
            if (hasSavedProgress && !resumeAutomatically) {
                const savedGameChoice = await showModal({
                    title: 'Progress Found',
                    message: 'You have an unfinished game at this level.',
                    info: [
                        { label: 'Attempts made', value: Number(data.attempts || 0) },
                        { label: 'Hints remaining', value: Number(data.hintsRemaining ?? 3) },
                        { label: 'Clades revealed', value: Array.isArray(data.revealedClades) ? data.revealedClades.length : 0 }
                    ],
                    buttons: [
                        { text: 'Continue Game', value: 'continue', primary: true },
                        { text: 'Start Fresh', value: 'fresh', primary: false }
                    ],
                    closeOnOverlay: false
                });

                if (savedGameChoice === 'fresh') {
                    localStorage.removeItem(storageKey);
                    data = null;
                }
            }
        } catch (error) {
            console.warn('Stored game session could not be restored:', error);
            localStorage.removeItem(storageKey);
        }
    }

    if (!data) {
        data = await callGameApi('start', { mode, difficulty });
        localStorage.setItem(storageKey, data.sessionId);
    }

    applyServerGamePayload(data);
    updateServerGameDisplay(data);
    initializeAutocomplete();
    document.getElementById('dino-input')?.focus();

    if (data.complete) await showRestoredServerCompletion(data);
}

async function loadChallengeDatabase(code, playerName) {
    const normalizedCode = String(code || '').toUpperCase().replace(/[^A-Z0-9]/g, '').slice(0, 6);
    const storageKey = getChallengeSessionStorageKey(normalizedCode);
    let data = null;
    const storedSessionId = localStorage.getItem(storageKey);

    if (storedSessionId) {
        try {
            const restored = await callGameApi('state', { sessionId: storedSessionId });
            if (restored.mode === 'challenge' && restored.challenge?.code === normalizedCode) {
                data = restored;
            } else {
                localStorage.removeItem(storageKey);
            }
        } catch (error) {
            localStorage.removeItem(storageKey);
        }
    }

    if (!data) {
        data = await callGameApi('join_challenge', {
            code: normalizedCode,
            playerName
        });
        localStorage.setItem(storageKey, data.sessionId);
    }

    await startFriendChallengeFromPayload(data);
}

async function restoreStoredChallenge(code) {
    const normalizedCode = String(code || '').toUpperCase().replace(/[^A-Z0-9]/g, '').slice(0, 6);
    if (!normalizedCode) return false;

    const storageKey = getChallengeSessionStorageKey(normalizedCode);
    const storedSessionId = localStorage.getItem(storageKey);
    if (!storedSessionId) return false;

    try {
        const data = await callGameApi('state', { sessionId: storedSessionId });
        if (data.mode !== 'challenge' || data.challenge?.code !== normalizedCode) {
            localStorage.removeItem(storageKey);
            return false;
        }
        await startFriendChallengeFromPayload(data);
        return true;
    } catch (error) {
        console.warn('Stored friend challenge could not be restored:', error);
        localStorage.removeItem(storageKey);
        return false;
    }
}

async function loadPracticeDatabase(difficulty, forceClean = true, resumeAutomatically = false) {
    try {
        await loadServerDatabase('practice', difficulty, forceClean, resumeAutomatically);
    } catch (error) {
        console.error('Server practice game error:', error);
        const wrapper = document.getElementById('tree-scroll-wrapper');
        if (wrapper) wrapper.innerHTML = `<div class="empty-state" style="color:#c62828;"><strong>Error loading challenge</strong><br>${error.message}</div>`;
    }
}

async function loadDailyDatabase(difficulty, forceClean = false, resumeAutomatically = false) {
    try {
        await loadServerDatabase('daily', difficulty, forceClean, resumeAutomatically);
    } catch (error) {
        console.error('Server daily game error:', error);
        const wrapper = document.getElementById('tree-scroll-wrapper');
        if (wrapper) wrapper.innerHTML = `<div class="empty-state" style="color:#c62828;"><strong>Error loading challenge</strong><br>${error.message}</div>`;
    }
}

// ═══════════════════════════════════════════════════════════════════════
// DISCOVERY GALLERY / MUSEUM OPERATIONS
// ═══════════════════════════════════════════════════════════════════════

const DISCOVERY_EVENTS_KEY = 'phylosaur-discovery-events-v1';

function readLocalDiscoveryNames() {
    try {
        const names = JSON.parse(localStorage.getItem('phylosaur-discoveries') || '[]');
        return Array.isArray(names) ? names.filter(Boolean) : [];
    } catch (error) {
        console.warn('Could not read the legacy discovery list:', error);
        return [];
    }
}

function readLocalDiscoveryEvents() {
    try {
        const events = JSON.parse(localStorage.getItem(DISCOVERY_EVENTS_KEY) || '[]');
        return Array.isArray(events) ? events : [];
    } catch (error) {
        console.warn('Could not read discovery history:', error);
        return [];
    }
}

function registerDiscovery(dinoName, museumProof = null) {
    if (!dinoName) {
        return { isFirstDiscovery: false, discoveryCount: 0, eventRecorded: false };
    }

    const normalizedDinoName = dinoName.trim().toLowerCase();
    const localDiscoveries = readLocalDiscoveryNames();
    const events = readLocalDiscoveryEvents();
    const wasAlreadyUnlocked =
        localDiscoveries.some(name => name.trim().toLowerCase() === normalizedDinoName) ||
        events.some(event => event?.dinoName?.trim().toLowerCase() === normalizedDinoName);

    if (!wasAlreadyUnlocked) {
        localDiscoveries.push(dinoName);
        localStorage.setItem('phylosaur-discoveries', JSON.stringify(localDiscoveries));
    }

    const discoveredAt = new Date().toISOString();
    const source = currentGameMode;
    const eventKey = gameSessionId
        ? `session:${gameSessionId}`
        : currentGameMode === 'daily'
            ? `daily:${getTodayString()}:${selectedDifficulty}`
            : currentGameMode === 'challenge'
                ? `challenge:${currentChallengeCode}:${dinoName}`
                : `practice:${discoveredAt}:${Math.random().toString(36).slice(2, 9)}`;
    const priorEventCount = events.filter(
        event => event?.dinoName?.trim().toLowerCase() === normalizedDinoName
    ).length;

    const existingEvent = events.find(event => event.eventKey === eventKey);
    let eventRecorded = false;

    if (!existingEvent) {
        events.push({
            eventKey,
            dinoName,
            discoveredAt,
            source,
            difficulty: selectedDifficulty,
            sessionId: gameSessionId || null,
            firstKnownUnlock: !wasAlreadyUnlocked,
            museumProof: museumProof || null
        });
        localStorage.setItem(DISCOVERY_EVENTS_KEY, JSON.stringify(events));
        eventRecorded = true;
    } else if (museumProof) {
        if (!existingEvent.museumProof) {
            existingEvent.museumProof = museumProof;
            localStorage.setItem(DISCOVERY_EVENTS_KEY, JSON.stringify(events));
        }
    }

    console.log(`Dinosaur discovery recorded: ${dinoName} (${source})`);
    const recordedEventCount = events.filter(
        event => event?.dinoName?.trim().toLowerCase() === normalizedDinoName
    ).length;
    const legacyDiscoveryBaseline = wasAlreadyUnlocked && priorEventCount === 0 ? 1 : 0;

    return {
        isFirstDiscovery: !wasAlreadyUnlocked,
        discoveryCount: Math.max(recordedEventCount + legacyDiscoveryBaseline, 1),
        eventRecorded
    };
}

async function getDiscoveryRecords() {
    const legacyNames = readLocalDiscoveryNames();
    const localEvents = readLocalDiscoveryEvents();
    const allEvents = new Map();
    const serverDinoNames = new Set();

    localEvents.forEach((event, index) => {
        if (!event?.dinoName) return;
        const eventKey = event.eventKey || `local:${index}:${event.dinoName}`;
        allEvents.set(eventKey, {
            eventKey,
            dinoName: event.dinoName,
            discoveredAt: event.discoveredAt || null,
            source: event.source || 'local',
            difficulty: event.difficulty || null,
            sessionId: event.sessionId || null,
            firstKnownUnlock: event.firstKnownUnlock === true,
            museumProof: event.museumProof || null
        });
    });

    if (currentUserId) {
        try {
            const progress = await callGameApi('account_discoveries');

            (progress.discoveries || []).forEach((row, index) => {
                if (!row.dinoName) return;
                const eventKey = row.eventKey || `account:${index}:${row.dinoName}`;
                serverDinoNames.add(row.dinoName.toLowerCase());
                const accountTime = row.discoveredAt ? new Date(row.discoveredAt).getTime() : NaN;
                const duplicateLocalEntry = [...allEvents.entries()].find(([, event]) => {
                    if (event.dinoName?.toLowerCase() !== row.dinoName.toLowerCase()) return false;
                    if ((event.source || 'local') !== (row.source || 'account')) return false;
                    if (event.difficulty && row.difficulty && event.difficulty !== row.difficulty) return false;
                    if (event.sessionId && row.sessionId) return event.sessionId === row.sessionId;
                    const localTime = event.discoveredAt ? new Date(event.discoveredAt).getTime() : NaN;
                    return Number.isFinite(accountTime) && Number.isFinite(localTime)
                        && Math.abs(accountTime - localTime) < 10 * 60 * 1000;
                });
                if (duplicateLocalEntry) allEvents.delete(duplicateLocalEntry[0]);
                allEvents.set(eventKey, {
                    eventKey,
                    dinoName: row.dinoName,
                    discoveredAt: row.discoveredAt || null,
                    source: row.source || 'account',
                    difficulty: row.difficulty || null,
                    sessionId: row.sessionId || null,
                    firstKnownUnlock: true
                });
            });
        } catch (err) {
            console.error('Error syncing discovery history from Supabase:', err);
        }
    }

    const records = {};
    const ensureRecord = dinoName => {
        const key = dinoName.toLowerCase();
        if (!records[key]) {
            records[key] = {
                name: dinoName,
                count: 0,
                firstDiscoveredAt: null,
                lastDiscoveredAt: null,
                firstDateUnknown: false,
                museumProof: null
            };
        }
        return records[key];
    };

    allEvents.forEach(event => {
        const record = ensureRecord(event.dinoName);
        record.count += 1;
        if (event.museumProof) record.museumProof = event.museumProof;

        if (event.discoveredAt) {
            const timestamp = new Date(event.discoveredAt).getTime();
            if (!Number.isNaN(timestamp)) {
                if (!record.firstDiscoveredAt || timestamp < new Date(record.firstDiscoveredAt).getTime()) {
                    record.firstDiscoveredAt = event.discoveredAt;
                }
                if (!record.lastDiscoveredAt || timestamp > new Date(record.lastDiscoveredAt).getTime()) {
                    record.lastDiscoveredAt = event.discoveredAt;
                }
            }
        }
    });

    legacyNames.forEach(dinoName => {
        const key = dinoName.toLowerCase();
        const matchingLocalEvents = localEvents.filter(
            event => event?.dinoName?.toLowerCase() === key
        );
        const hasKnownFirstUnlock = matchingLocalEvents.some(
            event => event.firstKnownUnlock === true
        );

        // The old list had neither dates nor counts. Preserve it as one
        // undated discovery only when newer data cannot already explain it.
        if (!serverDinoNames.has(key) && !hasKnownFirstUnlock) {
            const record = ensureRecord(dinoName);
            record.count += 1;
            record.firstDateUnknown = true;
        } else if (!records[key]) {
            const record = ensureRecord(dinoName);
            record.count = 1;
            record.firstDateUnknown = true;
        }
    });

    const unlockedNames = Object.values(records).map(record => record.name);
    localStorage.setItem('phylosaur-discoveries', JSON.stringify(unlockedNames));
    return records;
}

async function getUnlockedDinos() {
    const records = await getDiscoveryRecords();
    return Object.values(records).map(record => record.name);
}

async function syncDiscoveriesOnLogin() {
    if (currentUserId) {
        await getUnlockedDinos();
    }
}

async function claimGuestProgressOnLogin({ showNotice = false } = {}) {
    if (!currentUserId) return { claimedSessions: 0 };

    const sessionIds = getStoredGameSessionIds(100);
    const guestAchievementIds = getGuestAchievementIds();
    if (sessionIds.length === 0 && guestAchievementIds.length === 0) {
        await syncDiscoveriesOnLogin();
        return { claimedSessions: 0 };
    }

    try {
        const result = await callGameApi('claim_guest_progress', {
            sessionIds,
            guestAchievementIds
        });
        dailyCompletionCache = null;

        if (result.statistics) {
            userStats.gamesPlayed = Number(result.statistics.games_played || 0);
            userStats.gamesWon = Number(result.statistics.games_won || 0);
            userStats.totalGuesses = Number(result.statistics.total_guesses || 0);
            userStats.bestScore = result.statistics.best_score ?? null;
        }

        await syncDiscoveriesOnLogin();

        if (showNotice && (
            Number(result.claimedSessions || 0) > 0
            || (result.linkedAchievements || []).length > 0
        )) {
            await customAlert(
                'Progress Saved',
                'Progress and achievements from this browser are now linked to your account.'
            );
        }

        return result;
    } catch (error) {
        console.error('Guest progress could not be linked to the account:', error);
        return { claimedSessions: 0, error };
    }
}