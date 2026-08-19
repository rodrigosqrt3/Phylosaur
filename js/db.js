// ═══════════════════════════════════════════════════════════════════════
// DATABSE OPERATIONS
// ═══════════════════════════════════════════════════════════════════════
async function updateStatsAfterGame(won, guessCount, difficulty) {
  if (!currentUserId) return;

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

    checkAchievements(won, guessCount);
    }

async function checkAchievements(won, guessCount) {
    if (!currentUserId) return;
    
    const achievements = [
        { id: 'first_win', condition: () => userStats.gamesWon === 1 },
        { id: 'perfect_game', condition: () => won && guessCount <= 3 },
        { id: 'ten_wins', condition: () => userStats.gamesWon >= 10 },
        { id: 'fifty_wins', condition: () => userStats.gamesWon >= 50 },
        { id: 'hard_win', condition: () => won && selectedDifficulty === 'dificil' },
        { id: 'very_hard_win', condition: () => won && selectedDifficulty === 'muito_dificil' }
    ];

    const { data: existing } = await sb.from('achievements')
        .select('achievement_id')
        .eq('user_id', currentUserId);

    const unlocked = new Set(existing ? existing.map(a => a.achievement_id) : []);

    for (const ach of achievements) {
        if (ach.condition() && !unlocked.has(ach.id)) {
        await sb.from('achievements').insert({ user_id: currentUserId, achievement_id: ach.id });
        showAchievementNotification(ach.id);
        }
    }
}

function showAchievementNotification(name) {
    const achievementNames = {
        first_win: 'First Win',
        perfect_game: 'Three Guesses',
        ten_wins: '10 Wins',
        fifty_wins: '50 Wins',
        hard_win: 'Level IV',
        very_hard_win: 'Level V'
    };
    const displayName = achievementNames[name] || name;
    const notif = document.createElement('div');
    notif.style.cssText = `
        position: fixed;
        top: 20px;
        right: 20px;
        background: linear-gradient(135deg, #3d4a2f 0%, #2d3a1f 100%);
        color: #d4e5c9;
        padding: 20px 30px;
        border-radius: 8px;
        border: 2px solid #4a5d36;
        box-shadow: 0 4px 20px rgba(0,0,0,0.5);
        z-index: 10000;
        animation: slideIn 0.5s ease-out;
        font-family: Georgia, serif;
      `;
      notif.innerHTML = `
        <div style="font-size:0.9em; color:#a68a5a; margin-bottom:5px;">Achievement Unlocked!</div>
        <div style="font-size:1.2em; font-weight:600; letter-spacing:1px;">${displayName}</div>
      `;
      document.body.appendChild(notif);
      
      setTimeout(() => {
        notif.style.animation = 'slideOut 0.5s ease-in';
        setTimeout(() => notif.remove(), 500);
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
    const { data } = await sb.from('daily_results')
    .select('difficulty')
    .eq('user_id', currentUserId)
    .eq('played_date', today)
    .eq('won', true);
    
    const status = { muito_facil: false, facil: false, normal: false, dificil: false, muito_dificil: false };
    if (data) data.forEach(row => { status[row.difficulty] = true; });
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

function updateServerGameDisplay(data) {
    const bestMatch = guesses.length > 0
        ? Math.max(...guesses.map(guess => guess.proximity.matches))
        : 0;

    const attempts = document.getElementById('attempts');
    const hints = document.getElementById('hints');
    const best = document.getElementById('best-match');
    const clades = document.getElementById('clades-revealed');
    const possible = document.getElementById('possible-specimens');

    if (attempts) attempts.textContent = guesses.length;
    if (hints) hints.textContent = hintsRemaining;
    if (best) best.textContent = bestMatch;
    if (clades) clades.textContent = revealedClades.size;
    if (possible) possible.textContent = serverPossibleSpecimens;

    const wrapper = document.getElementById('tree-scroll-wrapper');
    if (data.tree && (guesses.length > 0 || hintHistory.length > 0 || data.complete)) {
        renderTreeSnapshot(data.tree);
    } else if (wrapper) {
        wrapper.innerHTML = '<div class="empty-state">The tree will appear after your first guess or hint.</div>';
    }

    updateCladeInfo();
    updateGuessHistory();
}

function showRestoredServerCompletion(data) {
    const input = document.getElementById('dino-input');
    if (input) input.disabled = true;
    document.querySelector('.btn-guess')?.setAttribute('disabled', true);
    document.querySelector('.btn-hint')?.setAttribute('disabled', true);
    document.querySelector('.btn-giveup')?.setAttribute('disabled', true);

    const container = document.getElementById('tree-container');
    if (!container || container.querySelector('.victory')) return;

    const panel = document.createElement('div');
    panel.className = 'victory';
    if (data.gaveUp) {
        panel.style.background = 'linear-gradient(135deg, #3d2318 0%, #2c1a12 100%)';
    }
    panel.innerHTML = `
        <h2 style="${data.gaveUp ? 'color:var(--color-danger);' : ''}">
            ${data.gaveUp ? 'ANSWER REVEALED' : 'CHALLENGE COMPLETE'}
        </h2>
        <div class="victory-dino">${data.target?.nome || ''}</div>
        <p style="font-size:0.95em; color:var(--color-muted); margin-top:8px; letter-spacing:1px;">
            ${guesses.length} ${guesses.length === 1 ? 'attempt' : 'attempts'}
        </p>
        <button class="btn-new-game" onclick="${isPracticeMode ? 'showPracticeMode()' : 'showDifficultySelection()'}">
            ${isPracticeMode ? 'Play Again' : 'Return to Level Selection'}
        </button>
    `;
    container.insertBefore(panel, container.firstChild);
}

async function loadServerDatabase(mode, difficulty, forceClean = false) {
    window.collapsedClades.clear();
    window.currentTreeSnapshot = null;
    isPracticeMode = mode === 'practice';
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
            if (hasSavedProgress) {
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

    if (data.complete) showRestoredServerCompletion(data);
}

async function loadPracticeDatabase(difficulty) {
    try {
        await loadServerDatabase('practice', difficulty, true);
    } catch (error) {
        console.error('Server practice game error:', error);
        const wrapper = document.getElementById('tree-scroll-wrapper');
        if (wrapper) wrapper.innerHTML = `<div class="empty-state" style="color:#c62828;"><strong>Error loading challenge</strong><br>${error.message}</div>`;
    }
}

async function loadDailyDatabase(difficulty, forceClean = false) {
    try {
        await loadServerDatabase('daily', difficulty, forceClean);
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
    if (!dinoName) return;

    const localDiscoveries = readLocalDiscoveryNames();
    const wasAlreadyUnlocked = localDiscoveries.some(
        name => name.toLowerCase() === dinoName.toLowerCase()
    );

    if (!wasAlreadyUnlocked) {
        localDiscoveries.push(dinoName);
        localStorage.setItem('phylosaur-discoveries', JSON.stringify(localDiscoveries));
    }

    const discoveredAt = new Date().toISOString();
    const source = isPracticeMode ? 'practice' : 'daily';
    const eventKey = isPracticeMode
        ? `practice:${discoveredAt}:${Math.random().toString(36).slice(2, 9)}`
        : `daily:${getTodayString()}:${selectedDifficulty}`;
    const events = readLocalDiscoveryEvents();

    if (!events.some(event => event.eventKey === eventKey)) {
        events.push({
            eventKey,
            dinoName,
            discoveredAt,
            source,
            difficulty: selectedDifficulty,
            firstKnownUnlock: !wasAlreadyUnlocked,
            museumProof: museumProof || null
        });
        localStorage.setItem(DISCOVERY_EVENTS_KEY, JSON.stringify(events));
    } else if (museumProof) {
        const matchingEvent = events.find(event => event.eventKey === eventKey);
        if (matchingEvent && !matchingEvent.museumProof) {
            matchingEvent.museumProof = museumProof;
            localStorage.setItem(DISCOVERY_EVENTS_KEY, JSON.stringify(events));
        }
    }

    console.log(`Dinosaur discovery recorded: ${dinoName} (${source})`);
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
            firstKnownUnlock: event.firstKnownUnlock === true,
            museumProof: event.museumProof || null
        });
    });

    if (currentUserId) {
        try {
            const { data, error } = await sb.from('daily_results')
                .select('target_dino, played_date, created_at, difficulty')
                .eq('user_id', currentUserId)
                .eq('won', true);

            if (error) throw error;

            (data || []).forEach((row, index) => {
                if (!row.target_dino) return;
                const eventKey = `daily:${row.played_date || index}:${row.difficulty || 'unknown'}`;
                serverDinoNames.add(row.target_dino.toLowerCase());
                allEvents.set(eventKey, {
                    eventKey,
                    dinoName: row.target_dino,
                    discoveredAt: row.created_at || row.played_date || null,
                    source: 'daily',
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