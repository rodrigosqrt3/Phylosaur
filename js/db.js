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
    const newBest    = (base.best_score === null || guessCount < base.best_score)
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
        <div style="font-size:1.2em; font-weight:600; letter-spacing:1px;">◆ ${name}</div>
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

async function loadPracticeDatabase(difficulty) {
    window.collapsedClades.clear();
    isPracticeMode = true;
    
    const wrapper = document.getElementById('tree-scroll-wrapper');
    if (wrapper) wrapper.innerHTML = '<div class="loading">Loading practice challenge...</div>';
    
    try {
    const res = await fetch('phylosaur_db.json');
    if (!res.ok) throw new Error('Failed to load database');
    
    fullDatabase = await res.json();
    database = fullDatabase.filter(d => d.dificuldade === difficulty);
    
    console.log(`Loaded ${database.length} specimens for practice mode (${difficulty})`);
    
    if (database.length === 0) {
        throw new Error(`No specimens found for difficulty: ${difficulty}`);
    }
    
    const randomIndex = Math.floor(Math.random() * database.length);
    targetDino = database[randomIndex];
    guesses = [];
    hintsRemaining = 3;
    isGiveUpMode = false;
    gameWon = false;
    guessedNames = new Set();
    revealedClades = new Set();
    hintHistory = [];
    guessesSinceLastHint = 0;
    
    console.log('Practice specimen:', targetDino.nome);
    
    document.getElementById('attempts').textContent = '0';
    document.getElementById('hints').textContent = '3';
    document.getElementById('best-match').textContent = '0';
    document.getElementById('clades-revealed').textContent = '0';
    
    if (wrapper) {
        wrapper.innerHTML = '<div class="empty-state">The phylogenetic tree will progressively reveal as you explore the evolutionary landscape through your classification attempts.</div>';
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
    if (wrapper) wrapper.innerHTML = '<div class="loading">Loading classification database...</div>';
    
    try {
    const res = await fetch('phylosaur_db.json');
    if (!res.ok) throw new Error('Failed to load database');
    
    fullDatabase = await res.json();
    database = fullDatabase.filter(d => d.dificuldade === difficulty);
    
    console.log(`Loaded ${database.length} specimens for difficulty: ${difficulty}`);
    
    if (database.length === 0) {
        throw new Error(`No specimens found for difficulty: ${difficulty}`);
    }
    
    const seed = getDailySeed(difficulty);
    const hash = hashString(seed);
    const index = hash % database.length;
    
    targetDino = database[index];
    guesses = [];
    hintsRemaining = 3;
    isGiveUpMode = false;
    gameWon = false;
    guessedNames = new Set();
    revealedClades = new Set();
    hintHistory = [];
    guessesSinceLastHint = 0;
    
    console.log('Daily specimen:', targetDino.nome, 'Seed:', seed);
    
    const savedProgress = forceClean ? null : await loadGameProgress(difficulty);

    if (savedProgress && savedProgress.targetDino === targetDino.nome) {
        console.log('Loading saved progress...');
        
        if (savedProgress.guesses && savedProgress.guesses.length > 0) {
        guesses = savedProgress.guesses.map(savedGuess => {
            const dino = database.find(d => d.nome === savedGuess.nome);
            if (!dino) return null;
            
            const proximity = calculateProximity(dino, targetDino);
            
            if (proximity.lastCommonClade) {
            revealedClades.add(proximity.lastCommonClade);
            }
            
            return {
            dino: dino,
            proximity: proximity,
            isHint: savedGuess.isHint || false
            };
        }).filter(g => g !== null);
        }
        
        if (savedProgress.revealedClades) {
        savedProgress.revealedClades.forEach(c => revealedClades.add(c));
        }
        hintHistory = savedProgress.hintHistory || [];
        hintsRemaining = savedProgress.hintsRemaining !== undefined ? savedProgress.hintsRemaining : 3;
        guessedNames = new Set((savedProgress.guesses || []).map(g => g.nome.toLowerCase()));
        
        console.log(`Restored ${guesses.length} guesses, ${hintsRemaining} hints remaining`);
        
        setTimeout(() => {
        const bestMatch = guesses.length > 0 
            ? Math.max(...guesses.map(g => g.proximity.matches)) 
            : 0;
        
        document.getElementById('attempts').textContent = guesses.length;
        document.getElementById('hints').textContent = hintsRemaining;
        document.getElementById('best-match').textContent = bestMatch;
        document.getElementById('clades-revealed').textContent = revealedClades.size;
        
        renderEnhancedTree();
        updateCladeInfo();
        updateGuessHistory();
        document.getElementById('dino-input')?.focus();
        }, 100);
        
    } else {
        document.getElementById('attempts').textContent = '0';
        document.getElementById('hints').textContent = '3';
        document.getElementById('best-match').textContent = '0';
        document.getElementById('clades-revealed').textContent = '0';
        
        if (wrapper) {
        wrapper.innerHTML = '<div class="empty-state">The phylogenetic tree will progressively reveal as you explore the evolutionary landscape through your classification attempts.</div>';
        }
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
    
    if (result.guesses && result.guesses.length > 0) {
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

function registerDiscovery(dinoName) {
    if (!dinoName) return;
    
    let localDiscoveries = JSON.parse(localStorage.getItem('phylosaur-discoveries') || '[]');
    if (!localDiscoveries.includes(dinoName)) {
        localDiscoveries.push(dinoName);
        localStorage.setItem('phylosaur-discoveries', JSON.stringify(localDiscoveries));
        console.log(`Dinosaur discovered and cataloged: ${dinoName}`);
    }
}

async function getUnlockedDinos() {
    let localDiscoveries = JSON.parse(localStorage.getItem('phylosaur-discoveries') || '[]');
    let uniqueDinos = new Set(localDiscoveries);

    if (currentUserId) {
        try {
            const { data, error } = await sb.from('daily_results')
                .select('target_dino')
                .eq('user_id', currentUserId)
                .eq('won', true);

            if (data && !error) {
                data.forEach(row => {
                    uniqueDinos.add(row.target_dino);
                });
                
                localStorage.setItem('phylosaur-discoveries', JSON.stringify(Array.from(uniqueDinos)));
            }
        } catch (err) {
            console.error('Error syncing discoveries from Supabase:', err);
        }
    }

    return Array.from(uniqueDinos);
}

async function syncDiscoveriesOnLogin() {
    if (currentUserId) {
        await getUnlockedDinos();
    }
}