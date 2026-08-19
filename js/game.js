// ═══════════════════════════════════════════════
// GAME INITIALIZATION AND MAIN LOGIC
// ═══════════════════════════════════════════════
async function startPracticeChallenge(difficulty) {
    setHeaderControls('practice');
    currentGameMode = 'practice';
    selectedDifficulty = difficulty;
    
    const appContent = document.getElementById('app-content');
    
    appContent.innerHTML = `

    <div style="text-align:center; padding:15px; background:rgba(139,115,85,0.2); border-radius:8px; margin-bottom:20px; border:2px solid var(--color-muted);">
        <div style="color:var(--color-accent); font-weight:600; letter-spacing:2px; font-size:1.1em;">
        PRACTICE MODE
        </div>
    </div>

    <div class="game-card">
        <div class="stats" style="grid-template-columns: repeat(5, 1fr); gap: 12px;">
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
        <div class="stat-label">Clades Shown</div>
        </div>
        <div class="stat">
        <div class="stat-value" id="possible-specimens">—</div>
        <div class="stat-label">Possible Answers</div>
        </div>
        </div>

        <div class="input-section">
        <div class="guess-primary-row">
            <div class="guess-field">
            <input type="text" id="dino-input" placeholder="Enter a dinosaur name..." autocomplete="off" />
            <div id="suggestions"></div>
            </div>
            <button class="btn-guess" onclick="makeGuess()">Submit</button>
        </div>
        <div class="guess-secondary-row">
            <button class="btn-hint btn-game-hint" onclick="useHint()">Hint</button>
            <button class="btn-giveup" onclick="giveUp()">Give Up</button>
        </div>
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
    currentGameMode = 'daily';
    selectedDifficulty = difficulty;
    const appContent = document.getElementById('app-content');
    
    appContent.innerHTML = `
    <div class="game-card">
        <div class="stats" style="grid-template-columns: repeat(5, 1fr); gap: 12px;">
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
        <div class="stat-label">Clades Shown</div>
        </div>
        <div class="stat">
        <div class="stat-value" id="possible-specimens">—</div>
        <div class="stat-label">Possible Answers</div>
        </div>
        </div>

        <div class="input-section">
        <div class="guess-primary-row">
            <div class="guess-field">
            <input type="text" id="dino-input" placeholder="Enter a dinosaur name..." autocomplete="off" />
            <div id="suggestions"></div>
            </div>
            <button class="btn-guess" onclick="makeGuess()">Submit</button>
        </div>
        <div class="guess-secondary-row">
            <button class="btn-hint btn-game-hint" onclick="useHint()">Hint</button>
            <button class="btn-giveup" onclick="giveUp()">Give Up</button>
        </div>
        </div>

        <div id="tree-container">
        <div id="tree-scroll-wrapper">
            <div class="empty-state">Loading daily challenge...</div>
        </div>
        </div>

        <div id="clade-info"></div>
        <div id="guess-history"></div>
    </div>
    `;

    await loadDailyDatabase(difficulty, false);
}

async function startFriendChallengeFromPayload(data) {
    setHeaderControls('challenge');
    window.collapsedClades.clear();
    window.currentTreeSnapshot = null;
    if (typeof resetTreeAnimationState === 'function') resetTreeAnimationState();
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
    currentGameMode = 'challenge';
    isPracticeMode = false;
    selectedDifficulty = data.difficulty;
    currentChallengeCode = data.challenge?.code || currentChallengeCode;
    currentChallengePlayerName = data.challenge?.playerName || currentChallengePlayerName || 'Explorer';
    currentChallengeCreatorName = data.challenge?.creatorName || currentChallengeCreatorName;
    currentChallengePlacement = data.challenge?.placement ?? null;
    currentChallengeTotalPlayers = Number(data.challenge?.totalPlayers || 0);
    currentChallengeEliminated = Boolean(data.challenge?.eliminated);
    challengeRaceClosing = false;

    const appContent = document.getElementById('app-content');
    appContent.innerHTML = `
    <div class="challenge-banner">
        <div>
            <span>Friend Challenge</span>
            <strong>${escapeChallengeHtml(currentChallengeCode)}</strong>
        </div>
        <div class="challenge-banner-actions">
            <button class="btn-hint btn-header" onclick="copyChallengeCode()">Copy Code</button>
            <button class="btn-hint btn-header" onclick="showChallengeStandings()">Standings</button>
        </div>
        <div class="challenge-race-progress" id="challenge-race-status">Checking the field…</div>
    </div>
    <div class="game-card">
        <div class="stats" style="grid-template-columns: repeat(5, 1fr); gap: 12px;">
            <div class="stat"><div class="stat-value" id="attempts">0</div><div class="stat-label">Attempts</div></div>
            <div class="stat"><div class="stat-value" id="hints">3</div><div class="stat-label">Hints</div></div>
            <div class="stat"><div class="stat-value" id="best-match">0</div><div class="stat-label">Deepest Node</div></div>
            <div class="stat"><div class="stat-value" id="clades-revealed">0</div><div class="stat-label">Clades Shown</div></div>
            <div class="stat"><div class="stat-value" id="possible-specimens">—</div><div class="stat-label">Possible Answers</div></div>
        </div>
        <div class="input-section">
            <div class="guess-primary-row">
                <div class="guess-field">
                    <input type="text" id="dino-input" placeholder="Enter a dinosaur name..." autocomplete="off">
                    <div id="suggestions"></div>
                </div>
                <button class="btn-guess" onclick="makeGuess()">Submit</button>
            </div>
            <div class="guess-secondary-row">
                <button class="btn-hint btn-game-hint" onclick="useHint()">Hint</button>
                <button class="btn-giveup" onclick="giveUp()">Give Up</button>
            </div>
        </div>
        <div id="tree-container"><div id="tree-scroll-wrapper"><div class="empty-state">Loading friend challenge...</div></div></div>
        <div id="clade-info"></div>
        <div id="guess-history"></div>
    </div>`;

    applyServerGamePayload(data);
    updateServerGameDisplay(data);
    initializeAutocomplete();
    document.getElementById('dino-input')?.focus();
    if (data.complete) {
        stopChallengeStatusPolling();
        await showRestoredServerCompletion(data);
    } else {
        startChallengeStatusPolling();
    }
}

function stopChallengeStatusPolling() {
    if (challengeStatusPollTimer) clearInterval(challengeStatusPollTimer);
    challengeStatusPollTimer = null;
}

function updateChallengeRaceStatus(data) {
    if (!data?.race) return;
    currentChallengePlacement = data.race.requesterPlacement ?? currentChallengePlacement;
    currentChallengeTotalPlayers = Number(data.race.totalPlayers || currentChallengeTotalPlayers || 0);
    const status = document.getElementById('challenge-race-status');
    if (status) {
        const total = Number(data.race.totalPlayers || 0);
        const completed = Number(data.race.completedPlayers || 0);
        status.textContent = total < 2
            ? 'Waiting for another explorer to join'
            : `${total} explorers · ${completed} finished`;
    }
}

async function handleChallengeRaceClosure(statusData) {
    if (challengeRaceClosing || currentGameMode !== 'challenge') return;
    challengeRaceClosing = true;
    stopChallengeStatusPolling();
    updateChallengeRaceStatus(statusData);
    currentChallengeEliminated = true;

    try {
        const state = await callGameApi('state', { sessionId: gameSessionId });
        applyServerGamePayload(state);
        currentChallengeEliminated = true;
        currentChallengePlacement = statusData.race?.requesterPlacement ||
            state.challenge?.placement || currentChallengeTotalPlayers;
        setTreeAnimationMode('reveal');
        updateServerGameDisplay(state);
        await showRestoredServerCompletion(state);
    } catch (error) {
        await customAlert('Race Complete', 'The remaining positions have been decided. Reload the challenge to see the final result.');
    }
}

async function pollChallengeRaceStatus() {
    if (currentGameMode !== 'challenge' || !currentChallengeCode || !gameSessionId || challengeRaceClosing) {
        stopChallengeStatusPolling();
        return;
    }

    try {
        const data = await callGameApi('challenge_status', {
            code: currentChallengeCode,
            sessionId: gameSessionId
        });
        updateChallengeRaceStatus(data);
        if (data.race?.closedRequester) {
            await handleChallengeRaceClosure(data);
        } else if (data.requesterComplete) {
            stopChallengeStatusPolling();
        }
    } catch (error) {
        console.warn('Challenge race status unavailable:', error);
    }
}

function startChallengeStatusPolling() {
    stopChallengeStatusPolling();
    pollChallengeRaceStatus();
    challengeStatusPollTimer = setInterval(pollChallengeRaceStatus, 4000);
}

async function refreshCurrentChallengePlacement() {
    if (currentGameMode !== 'challenge' || !currentChallengeCode || !gameSessionId) return null;
    stopChallengeStatusPolling();
    try {
        const data = await callGameApi('challenge_status', {
            code: currentChallengeCode,
            sessionId: gameSessionId
        });
        updateChallengeRaceStatus(data);
        return data;
    } catch (error) {
        console.warn('Could not refresh challenge placement:', error);
        return null;
    }
}

async function copyChallengeCode() {
    if (!currentChallengeCode) return;
    try {
        await navigator.clipboard.writeText(currentChallengeCode);
        await customAlert('Code Copied', `<strong class="challenge-code-inline">${escapeChallengeHtml(currentChallengeCode)}</strong><br><br>Send this code to your friends.`);
    } catch (error) {
        await customAlert('Challenge Code', `<strong class="challenge-code-inline">${escapeChallengeHtml(currentChallengeCode)}</strong>`);
    }
}

async function showChallengeStandings() {
    if (!currentChallengeCode || !gameSessionId) return;
    try {
        const data = await callGameApi('challenge_status', {
            code: currentChallengeCode,
            sessionId: gameSessionId
        });
        updateChallengeRaceStatus(data);
        if (data.race?.closedRequester) {
            await handleChallengeRaceClosure(data);
            return;
        }
        const rows = data.participants.map((participant, index) => {
            const status = participant.status === 'solved'
                ? 'Finished'
                : participant.status === 'eliminated'
                ? 'Race closed'
                : participant.status === 'gave_up'
                ? 'Gave up'
                : 'Playing';
            const details = data.requesterComplete
                ? `${participant.attempts} attempts · ${participant.hintsUsed} hints${participant.status === 'playing' ? ' · Playing' : ''}`
                : status;
            return `<div class="standing-row ${participant.isYou ? 'is-you' : ''}">
                <span class="standing-rank">${data.requesterComplete ? (participant.placement ? `#${participant.placement}` : '…') : '◆'}</span>
                <span class="standing-name">${escapeChallengeHtml(participant.name)}${participant.isYou ? ' (you)' : ''}</span>
                <span class="standing-result">${escapeChallengeHtml(details)}</span>
            </div>`;
        }).join('');
        await customAlert(
            `Challenge ${escapeChallengeHtml(currentChallengeCode)}`,
            `<div class="standings-list">${rows || '<p>No explorers have joined yet.</p>'}</div>${data.requesterComplete ? '' : '<p class="standings-lock">Detailed scores appear after you finish, preventing outside information from influencing your game.</p>'}`
        );
    } catch (error) {
        await customAlert('Could Not Load Standings', error.message);
    }
}

function redrawGameTree() {
    renderCurrentGameTree();
}

function setGuessRequestPending(pending) {
    gameRequestPending = pending;
    const input = document.getElementById('dino-input');
    const button = document.querySelector('.btn-guess');

    if (button) {
        button.textContent = pending ? 'Analyzing…' : 'Submit';
        button.disabled = pending || gameWon;
    }
    if (input) input.disabled = pending || gameWon;

    if (!pending && !gameWon && input) {
        requestAnimationFrame(() => {
            input.focus();
        });
    }
}

async function makeGuess() {
    if (gameWon || document.querySelector('[data-app-modal="true"]')) return;
    await makeServerGuess();
}

async function makeServerGuess() {
    if (gameRequestPending || document.querySelector('[data-app-modal="true"]')) return;

    const input = document.getElementById('dino-input');
    const guessName = input?.value.trim() || '';

    if (!guessName) {
        await customAlert('Enter a Name', 'Choose a dinosaur from the suggestions.');
        return;
    }

    const available = database.find(
        dinosaur => dinosaur.nome.toLowerCase() === guessName.toLowerCase()
    );
    if (!available) {
        await customAlert('Dinosaur Not Found', 'Choose a name from the autocomplete suggestions.');
        return;
    }

    if (guessedNames.has(available.nome.toLowerCase())) {
        await customAlert('Already Guessed', 'You have already tried this dinosaur.');
        return;
    }

    setGuessRequestPending(true);

    let data;
    try {
        data = await callGameApi('guess', {
            sessionId: gameSessionId,
            guess: available.nome
        });
    } catch (error) {
        setGuessRequestPending(false);
        await customAlert('Guess Not Accepted', error.message);
        return;
    }

    try {
        guesses.push({
            dino: { nome: data.guess.nome },
            proximity: {
                matches: Number(data.guess.matches || 0),
                percentage: Number(data.guess.percentage || 0),
                lastCommonClade: data.guess.lastCommonClade || null,
                divergenceDepth: Number(data.guess.divergenceDepth || data.guess.matches || 0)
            },
            isHint: false
        });
        guessedNames.add(data.guess.nome.toLowerCase());
        guessesSinceLastHint++;

        applyServerGamePayload(data);
        setTreeAnimationMode(
            data.won ? 'victory' : 'guess',
            data.guess?.nome ? `display:${data.guess.nome}` : null
        );
        updateServerGameDisplay(data);

        if (input) input.value = '';
        const suggestions = document.getElementById('suggestions');
        if (suggestions) suggestions.style.display = 'none';
        if (input) {
            input.setAttribute('aria-expanded', 'false');
            input.removeAttribute('aria-activedescendant');
        }

        if (data.won) await showVictory();
    } catch (error) {
        console.error('Error displaying accepted guess:', error);
        await customAlert(
            'Display Error',
            'Your guess was accepted and saved, but part of the result screen could not be displayed. Reloading the challenge will restore it.'
        );
    } finally {
        setGuessRequestPending(false);
    }
}

async function useHint() {
    await useServerHint();
}

async function useServerHint() {
    if (gameWon) return;

    try {
        const data = await callGameApi('hint', { sessionId: gameSessionId });
        applyServerGamePayload(data);
        guessesSinceLastHint = 0;
        setTreeAnimationMode(
            'hint',
            data.hint?.cladeName ? `node:${data.hint.cladeName}` : null
        );
        updateServerGameDisplay(data);

        await customAlert(
            'Hint',
            `The next clade in the lineage is:<br><br><strong style="color:var(--color-primary); font-size:1.2em;">${data.hint.cladeName}</strong>`
        );
        await showCladeInfo(data.hint.cladeName);
    } catch (error) {
        const missing = Number(error.data?.guessesRequired || 0);
        const message = missing > 0
            ? `Make <strong>${missing}</strong> more guess(es) before using another hint.`
            : error.message;
        await customAlert('Hint Not Available', message);
    }
}

async function loadResultMedia(dinoName) {
    try {
        let media = null;

        if (typeof getCachedDinoMedia === 'function') {
            media = await getCachedDinoMedia(dinoName);
        } else {
            const url = await fetchWikimediaImage(dinoName);
            if (url) media = { url, source: 'totaldino' };
        }

        if (!media?.url) return null;

        const defaultSourcePage = `https://commons.wikimedia.org/wiki/File:${encodeURIComponent(dinoName + ' TD.png')}`;
        const sourcePage = media.file_page || defaultSourcePage;
        const credit = typeof getMuseumMediaCredit === 'function'
            ? getMuseumMediaCredit(dinoName, media)
            : `Image source: <a href="${sourcePage}" target="_blank" rel="noopener">Wikimedia Commons</a>`;

        return {
            ...media,
            sourcePage,
            credit
        };
    } catch (error) {
        console.error('Result media error:', error);
        return null;
    }
}

function buildResultMediaMarkup(dinoName, media) {
    if (!media?.url) return '';

    return `
        <div class="victory-media">
            <img class="victory-media-image" src="${media.url}" alt="${dinoName}">
            <div class="victory-media-credit">${media.credit}</div>
        </div>
    `;
}

function bindResultMedia(panel, dinoName, media) {
    if (!media?.url) return;

    panel.querySelector('.victory-media-image')?.addEventListener('click', () => {
        openImageLightbox(
            media.url,
            dinoName,
            media.sourcePage,
            media.credit
        );
    });
}

function revealResultPanel(container, panel) {
    panel.setAttribute('tabindex', '-1');
    panel.setAttribute('role', 'region');
    panel.setAttribute('aria-label', 'Challenge result');

    // The tree is its own scroll container. Reset it first, then bring the
    // result panel into the phone viewport after the tree has re-rendered.
    container.scrollTop = 0;
    container.scrollLeft = 0;

    requestAnimationFrame(() => {
        container.scrollTop = 0;
        container.scrollLeft = 0;
        panel.scrollIntoView({ behavior: 'smooth', block: 'start' });

        try {
            panel.focus({ preventScroll: true });
        } catch (error) {
            panel.focus();
        }
    });
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

    try {
        const data = await callGameApi('give_up', { sessionId: gameSessionId });
        applyServerGamePayload(data);
    } catch (error) {
        await customAlert('Could Not Give Up', error.message);
        return;
    }

    gameWon = false;

    if (currentUser && currentGameMode === 'daily') {
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

    if (currentGameMode === 'challenge') {
        currentChallengeEliminated = false;
        await refreshCurrentChallengePlacement();
    }

    document.getElementById('dino-input').disabled = true;
    document.querySelector('.btn-guess').disabled = true;
    document.querySelector('.btn-game-hint').disabled = true;
    document.querySelector('.btn-giveup')?.setAttribute('disabled', true);

    const resultMedia = await loadResultMedia(targetDino.nome);

    const container = document.getElementById('tree-container');
    const v = document.createElement('div');
    v.className = 'victory';
    v.style.background = 'linear-gradient(135deg, #3d2318 0%, #2c1a12 100%)';

    v.innerHTML = `
        <h2 style="color:var(--color-danger);">ANSWER REVEALED</h2>
        <div class="victory-dino">${targetDino.nome}</div>
        <p style="font-size:0.95em; color:var(--color-muted); margin-top:8px; letter-spacing:1px;">
        ${guesses.length} ${guesses.length === 1 ? 'attempt' : 'attempts'} · gave up
        </p>

        ${buildResultMediaMarkup(targetDino.nome, resultMedia)}

        ${currentGameMode === 'challenge' && currentChallengePlacement ? `
        <div class="race-placement-card">
            <strong>#${currentChallengePlacement}</strong>
            <span>Your current race position. It becomes final when the race closes.</span>
        </div>` : ''}

        ${currentGameMode === 'challenge' ? `
        <button class="btn-hint" onclick="showChallengeStandings()" style="width:100%; margin-top:20px;">View Standings</button>
        <button class="btn-new-game" onclick="showFriendChallenges()">Return to Friend Challenges</button>` : `
        <button class="btn-new-game" onclick="${isPracticeMode ? 'showPracticeMode()' : 'showDifficultySelection()'}">
        ${isPracticeMode ? 'Play Again' : 'Return to Level Selection'}
        </button>`}
    `;

    container.insertBefore(v, container.firstChild);
    bindResultMedia(v, targetDino.nome, resultMedia);
    isGiveUpMode = true;
    gameWon = true;
    setTreeAnimationMode('reveal');
    redrawGameTree();
    updateCladeInfo();
    revealResultPanel(container, v);
}

async function showVictory() {
    registerDiscovery(targetDino.nome, currentMuseumProof);
    if (currentUser && currentGameMode === 'daily') {
        await clearGameProgress(selectedDifficulty);
    }

    let streakData = null;
    let milestone = null;

    if (currentUser && currentGameMode === 'daily') {
        streakData = await updateStreak();
        milestone = checkStreakMilestone(streakData.current);
        await updateStatsAfterGame(true, guesses.length, selectedDifficulty);
        await markDailyChallengeCompleted(selectedDifficulty);
    }

    if (currentGameMode === 'challenge') {
        currentChallengeEliminated = false;
        await refreshCurrentChallengePlacement();
    }

    document.getElementById('dino-input').disabled = true;
    document.querySelector('.btn-guess').disabled = true;
    document.querySelector('.btn-game-hint').disabled = true;
    document.querySelector('.btn-giveup')?.setAttribute('disabled', true);

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
    if (currentGameMode === 'challenge') {
        modeHTML = `
        <div class="challenge-result-note">
            Friend Challenge <strong>${escapeChallengeHtml(currentChallengeCode)}</strong> — Daily statistics not affected
        </div>
        ${currentChallengePlacement ? `
        <div class="race-placement-card">
            <strong>#${currentChallengePlacement}</strong>
            <span>Your finishing position</span>
        </div>` : ''}`;
    }

    let streakHTML = '';
    if (streakData && currentGameMode === 'daily') {
        if (milestone) {
        streakHTML = `
            <div class="streak-celebration" style="margin-top:25px; padding:25px; background:linear-gradient(135deg, rgba(255,149,0,0.2), rgba(255,69,0,0.2)); border-radius:8px; border:2px solid var(--color-warning);">
            <div class="streak-milestone-title" style="font-size:2.2em; color:var(--color-warning); margin-bottom:12px;">◆ ${milestone} DAY MILESTONE! ◆</div>
            <div style="font-size:1.1em; color:var(--color-primary); margin-bottom:8px;">Current Streak: ${streakData.current} days</div>
            <div style="font-size:0.9em; color:var(--color-secondary);">Best: ${streakData.best} days</div>
            </div>
        `;
        } else {
        streakHTML = `
            <div class="streak-celebration" style="margin-top:25px; padding:20px; background:var(--bg-panel); border-radius:8px; border:2px solid var(--color-muted);">
            <div class="streak-title" style="font-size:1.8em; color:var(--color-warning); margin-bottom:8px;">◆ ${streakData.current} Day Streak</div>
            <div style="font-size:0.9em; color:var(--color-secondary);">Best: ${streakData.best} days</div>
            </div>
        `;
        }
    }

    const resultMedia = await loadResultMedia(targetDino.nome);
        v.innerHTML = `
            ${modeHTML}
            <h2>CHALLENGE COMPLETE</h2>
            <div class="victory-dino">${targetDino.nome}</div>
            <p style="font-size:0.95em; color:var(--color-muted); margin-top:8px; letter-spacing:1px;">
            ${guesses.length} ${guesses.length === 1 ? 'attempt' : 'attempts'} · ${revealedClades.size} ${revealedClades.size === 1 ? 'clade' : 'clades'} revealed
            </p>

            ${buildResultMediaMarkup(targetDino.nome, resultMedia)}

            ${streakHTML}

            <button class="btn-hint" onclick="shareResult()" id="share-btn" 
                    style="width:100%; padding:15px; font-size:14px; letter-spacing:2px; margin-top:24px; margin-bottom:12px;">
            Share Result
            </button>
            ${currentGameMode === 'challenge' ? `
            <button class="btn-hint" onclick="showChallengeStandings()" style="width:100%; padding:15px; margin-bottom:12px;">View Standings</button>
            <button class="btn-new-game" onclick="showFriendChallenges()">Return to Friend Challenges</button>` : `
            <button class="btn-new-game" onclick="${isPracticeMode ? 'showPracticeMode()' : 'showDifficultySelection()'}">
            ${isPracticeMode ? 'Play Again' : 'Return to Level Selection'}
            </button>`}
        `;

    container.insertBefore(v, container.firstChild);
    bindResultMedia(v, targetDino.nome, resultMedia);
    redrawGameTree();
    updateCladeInfo();
    revealResultPanel(container, v);
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
    const mode  = currentGameMode === 'challenge'
        ? ` (Friend Challenge ${currentChallengeCode})`
        : isPracticeMode ? ' (Practice)' : '';

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
    const options = {
        year: 'numeric',
        month: 'long',
        day: 'numeric',
        timeZone: 'UTC'
    };
    return today.toLocaleDateString('en-US', options);
}

function startCountdown() {
    function update() {
        const timer = document.getElementById('countdown-timer');
        if (!timer) return;

        const now = new Date();
        const nextUtcMidnight = Date.UTC(
            now.getUTCFullYear(),
            now.getUTCMonth(),
            now.getUTCDate() + 1
        );

        const diff = nextUtcMidnight - now.getTime();

        const h = String(Math.floor(diff / 1000 / 60 / 60)).padStart(2, '0');
        const m = String(Math.floor(diff / 1000 / 60) % 60).padStart(2, '0');
        const s = String(Math.floor(diff / 1000) % 60).padStart(2, '0');

        timer.textContent = `${h}:${m}:${s}`;

        setTimeout(update, 1000);
    }

    update();
}