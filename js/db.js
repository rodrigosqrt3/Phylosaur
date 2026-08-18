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

async function saveGameProgress(difficulty) {
    if (!currentUserId || isPracticeMode) return;
    const today = getTodayString();
    const { error } = await sb.from('daily_results').upsert({
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
      
    if (error) console.error('Error saving progress:', error);
}

async function loadGameProgress(difficulty) {
    if (!currentUserId) return null;
    
    const today = getTodayString();
    
    const { data, error } = await sb.from('daily_results')
    .select('*')
    .eq('user_id', currentUserId)
    .eq('played_date', today)
    .eq('difficulty', difficulty)
    .single();
    
    if (error || !data) return null;
    
    return {
    guesses: data.guesses,
    revealedClades: data.revealed_clades,
    hintHistory: data.hint_history,
    hintsRemaining: 3 - (data.hint_history?.length || 0),
    guessedNames: data.guesses?.map(g => g.nome) || [],
    targetDino: data.target_dino
    };
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

    const yesterday = new Date();
    yesterday.setDate(yesterday.getDate() - 1);
    const yesterdayStr = `${yesterday.getFullYear()}-${String(yesterday.getMonth() + 1).padStart(2, '0')}-${String(yesterday.getDate()).padStart(2, '0')}`;

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

function getDailySeed(difficulty) {
    const today = new Date();
    const dateStr = `${today.getFullYear()}-${String(today.getMonth() + 1).padStart(2, '0')}-${String(today.getDate()).padStart(2, '0')}`;
    return `${dateStr}-${difficulty}`;
}

function hashString(str) {
    let hash = 0;
    for (let i = 0; i < str.length; i++) {
    const char = str.charCodeAt(i);
    hash = ((hash << 5) - hash) + char;
    hash = hash & hash;
    }
    return Math.abs(hash);
}

function getTodayString() {
    const today = new Date();
    return `${today.getFullYear()}-${String(today.getMonth() + 1).padStart(2, '0')}-${String(today.getDate()).padStart(2, '0')}`;
}

function getServerSessionKey(mode, difficulty) {
    const suffix = mode === 'daily' ? new Date().toISOString().slice(0, 10) : 'current';
    return `phylosaur-session-v1:${mode}:${difficulty}:${suffix}`;
}

function applyServerGameState(data) {
    serverBackedGame = true;
    currentSessionId = data.sessionId;
    currentPossibleSpecimens = Number(data.possibleSpecimens || 0);
    currentTargetDepth = Number(data.targetDepth || 0);

    database = (data.availableNames || []).map(nome => ({ nome }));
    fullDatabase = database;
    targetDino = data.target || null;
    guesses = (data.guesses || []).map(guess => ({
        dino: { nome: guess.nome },
        proximity: {
            matches: Number(guess.matches || 0),
            percentage: Number(guess.percentage || 0),
            lastCommonClade: guess.lastCommonClade || null,
            divergenceDepth: Number(guess.matches || 0)
        },
        isHint: Boolean(guess.isHint)
    }));
    hintsRemaining = Number(data.hintsRemaining ?? 3);
    guessesSinceLastHint = Number(data.guessesSinceHint || 0);
    revealedClades = new Set(data.revealedClades || []);
    hintHistory = data.hintHistory || [];
    guessedNames = new Set(guesses.map(guess => guess.dino.nome.toLowerCase()));
    isGiveUpMode = Boolean(data.gaveUp);
    gameWon = Boolean(data.complete);

    document.getElementById('attempts').textContent = String(guesses.length);
    document.getElementById('hints').textContent = String(hintsRemaining);
    document.getElementById('best-match').textContent = String(
        guesses.length ? Math.max(...guesses.map(guess => guess.proximity.matches)) : 0
    );
    document.getElementById('clades-revealed').textContent = String(revealedClades.size);
    document.getElementById('possible-specimens').textContent = String(currentPossibleSpecimens);
}

async function openServerGame(mode, difficulty, forceClean = false) {
    const storageKey = getServerSessionKey(mode, difficulty);
    let data = null;

    if (!forceClean) {
        const savedSessionId = localStorage.getItem(storageKey);
        if (savedSessionId) {
            try {
                data = await loadPhylosaurSession(savedSessionId);
                if (data.complete) data = null;
            } catch (error) {
                console.warn('Could not restore server game:', error);
                localStorage.removeItem(storageKey);
            }
        }
    } else {
        localStorage.removeItem(storageKey);
    }

    if (!data) {
        data = await startPhylosaurSession(mode, difficulty);
        data.guesses = [];
        data.revealedClades = [];
        data.hintHistory = [];
        data.hintsRemaining = 3;
        data.guessesSinceHint = 0;
        data.targetDepth = 0;
        data.complete = false;
        localStorage.setItem(storageKey, data.sessionId);
    }

    applyServerGameState(data);
    return data;
}

async function loadPracticeDatabase(difficulty) {
    window.collapsedClades.clear();
    isPracticeMode = true;
    
    const wrapper = document.getElementById('tree-scroll-wrapper');
    if (wrapper) wrapper.innerHTML = '<div class="loading">Loading practice game...</div>';
    
    try {
    const data = await openServerGame('practice', difficulty, true);
    console.log(`Started server practice game with ${data.possibleSpecimens} specimens (${difficulty})`);
    
    if (wrapper) {
        wrapper.innerHTML = '<div class="empty-state">The tree will appear after your first guess.</div>';
    }

    initializeAutocomplete();
    
    } catch (err) {
    console.error('Database error:', err);
    if (wrapper) {
        wrapper.innerHTML = `<div class="empty-state" style="color:#c62828;"><strong>Error loading challenge</strong><br>${err.message}</div>`;
    }
  }
}

async function loadDailyDatabase(difficulty, forceClean = false) {
    window.collapsedClades.clear();
    const wrapper = document.getElementById('tree-scroll-wrapper');
    if (wrapper) wrapper.innerHTML = '<div class="loading">Loading daily challenge...</div>';
    
    try {
    const data = await openServerGame('daily', difficulty, forceClean);
    console.log(`Opened server daily game with ${data.possibleSpecimens} specimens (${difficulty})`);

    if (guesses.length > 0 || hintHistory.length > 0) {
        renderEnhancedTree();
        updateCladeInfo();
        updateGuessHistory();
    } else if (wrapper) {
        wrapper.innerHTML = '<div class="empty-state">The tree will appear after your first guess.</div>';
    }
    
    initializeAutocomplete();
    document.getElementById('dino-input')?.focus();

    } catch (err) {
    console.error('Database error:', err);
    if (wrapper) {
        wrapper.innerHTML = `<div class="empty-state" style="color:#c62828;"><strong>Error loading challenge</strong><br>${err.message}</div>`;
    }
    }
}

async function loadCompletedChallengeTree(difficulty, result) {
    serverBackedGame = false;
    const wrapper = document.getElementById('tree-scroll-wrapper');
    
    try {
    const res = await fetch('phylosaur_db.json');
    if (!res.ok) throw new Error('Failed to load database');
    
    fullDatabase = await res.json();
    database = fullDatabase.filter(d => d.dificuldade === difficulty);
    
    targetDino = database.find(d => d.nome === result.targetDino);
    
    if (!targetDino) {
        targetDino = fullDatabase.find(d => d.nome === result.targetDino);
    }
    
    if (Array.isArray(result.guesses)) {
        guesses = result.guesses.map(savedGuess => {
        const dino = database.find(d => d.nome === savedGuess.nome);
        if (!dino) {
            console.warn(`Dinosaur ${savedGuess.nome} not found in database`);
            return null;
        }
        
        const proximity = calculateProximity(dino, targetDino);
        return {
            dino: dino,
            proximity: proximity,
            isHint: savedGuess.isHint || false
        };
        }).filter(g => g !== null);
        
        revealedClades = new Set(result.revealedClades || []);
        hintHistory = result.hintHistory || [];
        
    } else {
        wrapper.innerHTML = `
        <div class="empty-state" style="padding:80px 40px;">
            <h3 style="color:#c9a96e; margin-bottom:20px; font-size:1.4em;">Challenge Completed</h3>
            <p style="color:#a68a5a; line-height:1.8; margin-bottom:15px;">
            This challenge was completed before the detailed save system was implemented.
            </p>
            <p style="color:#8b7355; font-size:0.95em; line-height:1.8;">
            The phylogenetic tree from this session is not available for review.
            Complete the challenge again to see the full tree visualization.
            </p>
            <button class="btn-new-game" onclick="showDifficultySelection()" 
                    style="margin-top:30px; width:auto; padding:14px 28px;">
            Return to Level Selection
            </button>
        </div>
        `;
        return;
    }
    
    gameWon = true;
    isGiveUpMode = Boolean(result.gaveUp);
    hintsRemaining = 0;
    guessedNames = new Set(guesses.map(g => g.dino.nome.toLowerCase()));
    
    renderEnhancedTree();
    updateCladeInfo();
    updateGuessHistory();
    
    } catch (err) {
    console.error('Error loading completed challenge:', err);
    if (wrapper) {
        wrapper.innerHTML = `<div class="empty-state" style="color:#c62828;"><strong>Error loading tree</strong><br>${err.message}</div>`;
    }
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

function registerDiscovery(dinoName) {
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
            firstKnownUnlock: !wasAlreadyUnlocked
        });
        localStorage.setItem(DISCOVERY_EVENTS_KEY, JSON.stringify(events));
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
            firstKnownUnlock: event.firstKnownUnlock === true
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
                firstDateUnknown: false
            };
        }
        return records[key];
    };

    allEvents.forEach(event => {
        const record = ensureRecord(event.dinoName);
        record.count += 1;

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