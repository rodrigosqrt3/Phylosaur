// ═══════════════════════════════════════════════
// GAME INITIALIZATION AND MAIN LOGIC
// ═══════════════════════════════════════════════
async function startPracticeChallenge(difficulty) {
    setHeaderControls('practice');
    selectedDifficulty = difficulty;
    
    const appContent = document.getElementById('app-content');
    
    appContent.innerHTML = `

    <div style="text-align:center; padding:15px; background:rgba(139,115,85,0.2); border-radius:8px; margin-bottom:20px; border:2px solid var(--color-muted);">
        <div style="color:var(--color-accent); font-weight:600; letter-spacing:2px; font-size:1.1em;">
        PRACTICE MODE
        </div>
    </div>

    <div class="game-card">
        <div class="stats stats-game">
        <div class="stat">
            <div class="stat-value" id="attempts">0</div>
            <div class="stat-label">Attempts</div>
        </div>
        <div class="stat">
            <div class="stat-value" id="hints">3</div>
            <div class="stat-label">Hints</div>
        </div>
        <div class="stat">
            <div class="stat-value" id="best-match">0</div>
            <div class="stat-label">Deepest Node</div>
        </div>
        <div class="stat">
        <div class="stat-value" id="clades-revealed">0</div>
        <div class="stat-label">Clades Revealed</div>
        </div>
        <div class="stat">
        <div class="stat-value" id="possible-specimens">—</div>
        <div class="stat-label">Possible</div>
        </div>
        </div>

        <div class="input-section">
        <input type="text" id="dino-input" placeholder="Enter specimen name..." autocomplete="off" />
        <button class="btn-guess" onclick="makeGuess()">Submit</button>
        <button class="btn-hint" onclick="useHint()">Hint</button>
        <button class="btn-hint" onclick="giveUp()" 
                style="opacity:0.6; font-size:12px; padding:10px 16px; letter-spacing:1px;">
        Give Up
        </button>
        <div id="suggestions"></div>
        </div>

        <div id="tree-container">
        <div id="tree-scroll-wrapper">
            <div class="empty-state">Loading practice challenge...</div>
        </div>
        </div>

        <div id="clade-info"></div>
        <div id="guess-history"></div>
    </div>
    `;
    loadPracticeDatabase(difficulty);
}

async function startDailyChallenge(difficulty) {
    setHeaderControls('game');
    isPracticeMode = false;
    const today = getTodayString();
    let existingResult = null;

    if (currentUserId) {
    const { data } = await sb.from('daily_results')
        .select('*')
        .eq('user_id', currentUserId)
        .eq('played_date', today)
        .eq('difficulty', difficulty)
        .single();
    existingResult = data;
    }

    if (existingResult && (existingResult.won || existingResult.gave_up)) {
        showCompletedChallenge(difficulty, {
            targetDino: existingResult.target_dino,
            guessCount: existingResult.guess_count,
            guesses: existingResult.guesses,
            revealedClades: existingResult.revealed_clades,
            hintHistory: existingResult.hint_history,
            gaveUp: existingResult.gave_up || false
        });
        return;
    }
    
    selectedDifficulty = difficulty;
    const appContent = document.getElementById('app-content');
    
    appContent.innerHTML = `
    <div class="game-card">
        <div class="stats stats-game">
        <div class="stat">
            <div class="stat-value" id="attempts">0</div>
            <div class="stat-label">Attempts</div>
        </div>
        <div class="stat">
            <div class="stat-value" id="hints">3</div>
            <div class="stat-label">Hints</div>
        </div>
        <div class="stat">
            <div class="stat-value" id="best-match">0</div>
            <div class="stat-label">Deepest Node</div>
        </div>
        <div class="stat">
        <div class="stat-value" id="clades-revealed">0</div>
        <div class="stat-label">Clades Revealed</div>
        </div>
        <div class="stat">
        <div class="stat-value" id="possible-specimens">—</div>
        <div class="stat-label">Possible</div>
        </div>
        </div>

        <div class="input-section">
        <input type="text" id="dino-input" placeholder="Enter specimen name..." autocomplete="off" />
        <button class="btn-guess" onclick="makeGuess()">Submit</button>
        <button class="btn-hint" onclick="useHint()">Hint</button>
        <button class="btn-hint" onclick="giveUp()" 
                style="opacity:0.6; font-size:12px; padding:10px 16px; letter-spacing:1px;">
        Give Up
        </button>
        <div id="suggestions"></div>
        </div>

        <div id="tree-container">
        <div id="tree-scroll-wrapper">
            <div class="empty-state">Loading classification challenge...</div>
        </div>
        </div>

        <div id="clade-info"></div>
        <div id="guess-history"></div>
    </div>
    `;

const savedProgress = loadGameProgress(difficulty);
let continueGame = true;

if (savedProgress && savedProgress.guesses && savedProgress.guesses.length > 0) {
    continueGame = await showModal({
    title: 'Progress Found',
    message: 'You have unfinished research for this classification challenge.',
    info: [
        { label: 'Attempts made', value: savedProgress.guesses.length },
        { label: 'Hints remaining', value: savedProgress.hintsRemaining },
        { label: 'Clades revealed', value: savedProgress.revealedClades?.length || 0 }
    ],
    buttons: [
        { text: 'Continue Research', value: 'continue', primary: true },
        { text: 'Start Fresh', value: 'fresh', primary: false }
    ]
    });
    
    if (continueGame === 'fresh') {
    clearGameProgress(difficulty);
    loadDailyDatabase(difficulty, true);
    return;
    }
}
loadDailyDatabase(difficulty, false);
}

function calculateProximity(guess, target) {
    let matches = 0;
    const minLength = Math.min(guess.linhagem.length, target.linhagem.length);
    
    for (let i = 0; i < minLength; i++) {
    if (guess.linhagem[i] === target.linhagem[i]) {
        matches++;
    } else {
        break;
    }
    }
    
    return {
    matches,
    percentage: Math.round((matches / target.linhagem.length) * 100),
    lastCommonClade: matches > 0 ? guess.linhagem[matches - 1] : null,
    divergenceDepth: matches
    };
}

function countPossibleSpecimens() {
    if (guesses.length === 0) return database.length;
    
    const revealedOnPath = [];
    
    guesses.forEach(guess => {
        const mrca = findMRCA(guess.dino.linhagem, targetDino.linhagem);
        if (mrca) {
        const idx = targetDino.linhagem.indexOf(mrca.clade);
        targetDino.linhagem.slice(0, idx + 1).forEach(c => {
            if (!revealedOnPath.includes(c)) revealedOnPath.push(c);
        });
        }
    });

    revealedClades.forEach(clade => {
        const idx = targetDino.linhagem.indexOf(clade);
        if (idx !== -1) {
        targetDino.linhagem.slice(0, idx + 1).forEach(c => {
            if (!revealedOnPath.includes(c)) revealedOnPath.push(c);
        });
        }
    });

    if (revealedOnPath.length === 0) return database.length;

    return database.filter(d => 
        revealedOnPath.every(clade => d.linhagem.includes(clade))
    ).length;
}

async function makeGuess() {
    if (gameWon) return;

    const input = document.getElementById('dino-input');
    const guessName = input.value.trim();

    if (!guessName) {
        await customAlert('Input Required', 'Please enter a specimen name to classify.');
        return;
    }

    const guessDino = database.find(d => d.nome.toLowerCase() === guessName.toLowerCase());

    if (!guessDino) {
        await customAlert('Taxon Not Found', 'This specimen is not in the classification database. Please use the autocomplete suggestions.');
        return;
    }

    if (guessedNames.has(guessDino.nome.toLowerCase())) {
        await customAlert('Duplicate Classification', 'This specimen has already been classified in this session.');
        return;
    }

    guessedNames.add(guessDino.nome.toLowerCase());
    guessesSinceLastHint++; 

    const proximity = calculateProximity(guessDino, targetDino);

    if (proximity.lastCommonClade) {
        revealedClades.add(proximity.lastCommonClade);
    }

    guesses.push({ 
        dino: guessDino, 
        proximity, 
        isHint: false 
    });

    document.getElementById('attempts').textContent = guesses.length;
    document.getElementById('best-match').textContent = Math.max(...guesses.map(g => g.proximity.matches));
    document.getElementById('clades-revealed').textContent = revealedClades.size;
    document.getElementById('possible-specimens').textContent = countPossibleSpecimens();

    if (guessDino.nome === targetDino.nome) {
        gameWon = true;
        showVictory();
        input.value = '';
        document.getElementById('suggestions').style.display = 'none';
        return; 
    }
    renderEnhancedTree();
    updateCladeInfo();
    updateGuessHistory();

    input.value = '';
    document.getElementById('suggestions').style.display = 'none';

    if (currentUser && !gameWon && !isPracticeMode) {
        saveGameProgress(selectedDifficulty);
    }
}

async function useHint() {
    if (hintsRemaining <= 0) {
    await customAlert('No Hints Available', 'You have exhausted all hints for this challenge.');
    return;
    }
    
    if (gameWon) return;

    const requiredGuesses = 2;
    if (guessesSinceLastHint < requiredGuesses && guesses.length > 0) {
        await customAlert(
        'Hint Cooldown', 
        `You must make <strong>${requiredGuesses - guessesSinceLastHint}</strong> more classification attempt(s) before requesting another hint.`
        );
        return;
    }

    const dinosauriaIndex = targetDino.linhagem.indexOf('Dinosauria');
    let maxRevealedIndex = dinosauriaIndex;
    
    if (guesses.length > 0) {
    guesses.forEach(guess => {
        const guessDepth = guess.proximity.matches - 1;
        if (guessDepth > maxRevealedIndex) {
        maxRevealedIndex = guessDepth;
        }
    });
    }
    
    revealedClades.forEach(clade => {
    if (targetDino.linhagem.includes(clade)) {
        const cladeIndex = targetDino.linhagem.indexOf(clade);
        if (cladeIndex > maxRevealedIndex) {
        maxRevealedIndex = cladeIndex;
        }
    }
    });
    const nextCladeIndex = maxRevealedIndex + 1;
    if (nextCladeIndex >= targetDino.linhagem.length) {
    await customAlert('Lineage Complete', 'The complete phylogenetic path has been revealed. No additional hints available.');
    return;
    }

    hintsRemaining--;
    document.getElementById('hints').textContent = hintsRemaining;

    const revealClade = targetDino.linhagem[nextCladeIndex];
    
    await customAlert('Hint', `The next clade in the lineage is:<br><br><strong style="color:var(--color-primary); font-size:1.2em;">${revealClade}</strong>`);

    revealedClades.add(revealClade);

    hintHistory.push({
    cladeName: revealClade,
    depth: nextCladeIndex + 1
    });
    
    document.getElementById('clades-revealed').textContent = revealedClades.size;
    document.getElementById('possible-specimens').textContent = countPossibleSpecimens();

    guessesSinceLastHint = 0;
    
    renderEnhancedTree();
    updateGuessHistory();
    await showCladeInfo(revealClade);

if (currentUser && !isPracticeMode) {
saveGameProgress(selectedDifficulty);
}
}

async function giveUp() {
    if (gameWon) return;

    const confirm = await customConfirm(
        'Give Up?',
        `Are you sure you want to reveal the answer? This will count as a loss.`,
        'Give Up',
        'Keep Trying'
    );

    if (confirm !== 'true') return;

    gameWon = false;

    if (currentUser && !isPracticeMode) {
        await updateStatsAfterGame(false, guesses.length, selectedDifficulty);
        await sb.from('daily_results').upsert({
            user_id: currentUserId,
            played_date: getTodayString(),
            difficulty: selectedDifficulty,
            target_dino: targetDino.nome,
            guess_count: guesses.length,
            won: false,
            gave_up: true,
            guesses: guesses.map(g => ({ nome: g.dino.nome, isHint: g.isHint || false })),
            revealed_clades: Array.from(revealedClades),
            hint_history: hintHistory
        }, { onConflict: 'user_id,played_date,difficulty' });
    }

    document.getElementById('dino-input').disabled = true;
    document.querySelector('.btn-guess').disabled = true;
    document.querySelector('.btn-hint').disabled = true;
    document.querySelector('.btn-giveup')?.setAttribute('disabled', true);

    const imageUrl = await fetchWikimediaImage(targetDino.nome);
    const commonsPage = `https://commons.wikimedia.org/wiki/File:${encodeURIComponent(targetDino.nome + ' TD.png')}`;

    const container = document.getElementById('tree-container');
    const v = document.createElement('div');
    v.className = 'victory';
    v.style.background = 'linear-gradient(135deg, #3d2318 0%, #2c1a12 100%)';

    v.innerHTML = `
        <h2 style="color:var(--color-danger);">BETTER LUCK TOMORROW</h2>
        <div class="victory-dino">${targetDino.nome}</div>
        <p style="font-size:0.95em; color:var(--color-muted); margin-top:8px; letter-spacing:1px;">
        ${guesses.length} ${guesses.length === 1 ? 'attempt' : 'attempts'} · gave up
        </p>

        ${imageUrl ? `
        <div style="max-width:440px; margin:24px auto 0;">
        <img src="${imageUrl}" alt="${targetDino.nome}" 
            style="width:100%; border:2px solid var(--border-subtle); 
                    border-radius:8px; display:block;
                    box-shadow:0 4px 16px rgba(0,0,0,0.5);
                    cursor:zoom-in;
                    transition:transform 0.2s, box-shadow 0.2s;"
            onmouseover="this.style.transform='scale(1.02)'; this.style.boxShadow='0 8px 24px rgba(0,0,0,0.7)'"
            onmouseout="this.style.transform='scale(1)'; this.style.boxShadow='0 4px 16px rgba(0,0,0,0.5)'"
            onclick="openImageLightbox('${imageUrl}', '${targetDino.nome}', '${commonsPage}')" />
        <div style="text-align:center; margin-top:8px; font-size:0.78em; color:var(--border-subtle); font-style:italic; letter-spacing:1px;">
            Art by <a href="https://totaldino.com" target="_blank" style="color:var(--color-muted); text-decoration:none;">TotalDino</a>
            · <a href="https://totaldino.com/dino/${targetDino.nome.toLowerCase()}" target="_blank" style="color:var(--color-muted); text-decoration:none;">View on TotalDino</a>
            · <a href="${commonsPage}" target="_blank" style="color:var(--color-muted); text-decoration:none;">Wikimedia Commons</a>
        </div>
        </div>` : ''}

        <button class="btn-new-game" onclick="${isPracticeMode ? 'showPracticeMode()' : 'showDifficultySelection()'}">
        ${isPracticeMode ? 'Practice Another' : 'Return to Level Selection'}
        </button>
    `;

    container.insertBefore(v, container.firstChild);
    isGiveUpMode = true;
    gameWon = true;
    renderEnhancedTree();
    updateCladeInfo();
}

async function showVictory() {
    if (currentUser && !isPracticeMode) {
        await clearGameProgress(selectedDifficulty);
    }

    let streakData = null;
    let milestone = null;

    if (currentUser && !isPracticeMode) {
        streakData = await updateStreak();
        milestone = checkStreakMilestone(streakData.current);
        await updateStatsAfterGame(true, guesses.length, selectedDifficulty);
        await markDailyChallengeCompleted(selectedDifficulty);
    }

    document.getElementById('dino-input').disabled = true;
    document.querySelector('.btn-guess').disabled = true;
    document.querySelector('.btn-hint').disabled = true;

    const container = document.getElementById('tree-container');
    const v = document.createElement('div');
    v.className = 'victory';

    let modeHTML = '';
    if (isPracticeMode) {
        modeHTML = `
        <div style="padding:15px; background:rgba(139,115,85,0.15); border-radius:6px; margin-bottom:20px; border:2px solid var(--color-muted);">
            <div style="color:var(--color-accent); font-weight:600; letter-spacing:1px; font-size:0.95em;">
            Practice Mode — Statistics not recorded
            </div>
        </div>
        `;
    }

    let streakHTML = '';
    if (streakData && !isPracticeMode) {
        if (milestone) {
        streakHTML = `
            <div style="margin-top:25px; padding:25px; background:linear-gradient(135deg, rgba(255,149,0,0.2), rgba(255,69,0,0.2)); border-radius:8px; border:2px solid var(--color-warning);">
            <div style="font-size:2.2em; color:var(--color-warning); margin-bottom:12px;">◆ ${milestone} DAY MILESTONE! ◆</div>
            <div style="font-size:1.1em; color:var(--color-primary); margin-bottom:8px;">Current Streak: ${streakData.current} days</div>
            <div style="font-size:0.9em; color:var(--color-secondary);">Best: ${streakData.best} days</div>
            </div>
        `;
        } else {
        streakHTML = `
            <div style="margin-top:25px; padding:20px; background:var(--color-bg-panel); border-radius:8px; border:2px solid var(--color-muted);">
            <div style="font-size:1.8em; color:var(--color-warning); margin-bottom:8px;">◆ ${streakData.current} Day Streak</div>
            <div style="font-size:0.9em; color:var(--color-secondary);">Best: ${streakData.best} days</div>
            </div>
        `;
        }
    }

    const imageUrl = await fetchWikimediaImage(targetDino.nome);
    const commonsPage = `https://commons.wikimedia.org/wiki/File:${encodeURIComponent(targetDino.nome + ' TD.png')}`;
        v.innerHTML = `
            ${modeHTML}
            <h2>PHYLOGENETIC CLASSIFICATION COMPLETE</h2>
            <div class="victory-dino">${targetDino.nome}</div>
            <p style="font-size:0.95em; color:var(--color-muted); margin-top:8px; letter-spacing:1px;">
            ${guesses.length} ${guesses.length === 1 ? 'attempt' : 'attempts'} · ${revealedClades.size} ${revealedClades.size === 1 ? 'clade' : 'clades'} revealed
            </p>

            ${imageUrl ? `
            <div style="max-width:440px; margin:24px auto 0;">
            <img src="${imageUrl}" alt="${targetDino.nome}" 
                style="width:100%; border:2px solid var(--border-subtle); 
                        border-radius:8px; display:block;
                        box-shadow:0 4px 16px rgba(0,0,0,0.5);
                        cursor:zoom-in;
                        transition:transform 0.2s, box-shadow 0.2s;"
                onmouseover="this.style.transform='scale(1.02)'; this.style.boxShadow='0 8px 24px rgba(0,0,0,0.7)'"
                onmouseout="this.style.transform='scale(1)'; this.style.boxShadow='0 4px 16px rgba(0,0,0,0.5)'"
                onclick="openImageLightbox('${imageUrl}', '${targetDino.nome}', '${commonsPage}')" />
            <div style="text-align:center; margin-top:8px; font-size:0.78em; color:var(--border-subtle); font-style:italic; letter-spacing:1px;">
                Art by <a href="https://totaldino.com" target="_blank" style="color:var(--color-muted); text-decoration:none;">TotalDino</a>
                · <a href="https://totaldino.com/dino/${targetDino.nome.toLowerCase()}" target="_blank" style="color:var(--color-muted); text-decoration:none;">View on TotalDino</a>
                · <a href="${commonsPage}" target="_blank" style="color:var(--color-muted); text-decoration:none;">Wikimedia Commons</a>
            </div>
            </div>` : ''}

            ${streakHTML}

            <button class="btn-hint" onclick="shareResult()" id="share-btn" 
                    style="width:100%; padding:15px; font-size:14px; letter-spacing:2px; margin-top:24px; margin-bottom:12px;">
            Share Result
            </button>
            <button class="btn-new-game" onclick="${isPracticeMode ? 'showPracticeMode()' : 'showDifficultySelection()'}">
            ${isPracticeMode ? 'Practice Another' : 'Return to Level Selection'}
            </button>
        `;

    container.insertBefore(v, container.firstChild);
    renderEnhancedTree();
    updateCladeInfo();
}

function shareResult() {
    const diffNames = {
        'muito_facil': 'Level I',
        'facil':       'Level II',
        'normal':      'Level III',
        'dificil':     'Level IV',
        'muito_dificil': 'Level V'
    };

    const blocks = guesses.map(g => {
        const pct = g.proximity.percentage;
        if (pct === 100)  return '🟩'; 
        if (pct >= 75)    return '🟨'; 
        if (pct >= 50)    return '🟧'; 
        if (pct >= 25)    return '🟥'; 
        return '⬛';                   
    });

    const rows = [];
    for (let i = 0; i < blocks.length; i += 8) {
        rows.push(blocks.slice(i, i + 8).join(''));
    }

    const today = getCurrentDateFormatted();
    const diff  = diffNames[selectedDifficulty] || '';
    const mode  = isPracticeMode ? ' (Practice)' : '';

    const text = [
        `Phylosaur — ${diff}${mode}`,
        `${today}`,
        ``,
        rows.join('\n'),
        ``,
        `${guesses.length} attempts • ${revealedClades.size} clades revealed`,
        ``,
        `https://rodrigosqrt3.github.io/Phylosaur/`
    ].join('\n');

    if (navigator.share) {
        navigator.share({ text }).catch(() => {});
    } else {
        navigator.clipboard.writeText(text).then(() => {
        const btn = document.getElementById('share-btn');
        if (!btn) return;
        const original = btn.textContent;
        btn.textContent = 'Copied to clipboard!';
        btn.disabled = true;
        setTimeout(() => {
            btn.textContent = original;
            btn.disabled = false;
        }, 2000);
        }).catch(() => {
        const ta = document.createElement('textarea');
        ta.value = text;
        ta.style.position = 'fixed';
        ta.style.opacity = '0';
        document.body.appendChild(ta);
        ta.select();
        document.execCommand('copy');
        document.body.removeChild(ta);

        const btn = document.getElementById('share-btn');
        if (btn) {
            btn.textContent = 'Copied!';
            btn.disabled = true;
            setTimeout(() => {
            btn.textContent = 'Share Result';
            btn.disabled = false;
            }, 2000);
        }
        });
    }
}

function getCurrentDateFormatted() {
    const today = new Date();
    const options = { year: 'numeric', month: 'long', day: 'numeric' };
    return today.toLocaleDateString('en-US', options);
}

function startCountdown() {
    function update() {
        const timer = document.getElementById('countdown-timer');
        if (!timer) return;

        const now = new Date();
        const midnight = new Date();
        midnight.setHours(24, 0, 0, 0);

        const diff = midnight - now;

        const h = String(Math.floor(diff / 1000 / 60 / 60)).padStart(2, '0');
        const m = String(Math.floor(diff / 1000 / 60) % 60).padStart(2, '0');
        const s = String(Math.floor(diff / 1000) % 60).padStart(2, '0');

        timer.textContent = `${h}:${m}:${s}`;

        setTimeout(update, 1000);
    }

    update();
}