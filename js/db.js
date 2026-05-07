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