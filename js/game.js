// ═══════════════════════════════════════════════
// GAME INITIALIZATION AND MAIN LOGIC
// ═══════════════════════════════════════════════
async function startPracticeChallenge(difficulty, { restoreExisting = false } = {}) {
    setAppRoute(`/game/practice/${difficulty}`);
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
        <div class="stat-value" id="possible-specimens">-</div>
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
            <button class="btn-hint btn-game-hint" onclick="useHint()" disabled title="Make 2 guesses before using a hint">Hint · 2 guesses</button>
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
    await loadPracticeDatabase(difficulty, !restoreExisting, restoreExisting);
}

async function startDailyChallenge(difficulty, { restoreExisting = false } = {}) {
    setAppRoute(`/game/daily/${difficulty}`);
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
        <div class="stat-value" id="possible-specimens">-</div>
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
            <button class="btn-hint btn-game-hint" onclick="useHint()" disabled title="Make 2 guesses before using a hint">Hint · 2 guesses</button>
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

    await loadDailyDatabase(difficulty, false, restoreExisting);
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
    if (currentChallengeCode) setAppRoute(`/challenge/${currentChallengeCode}`);
    currentChallengePlayerName = data.challenge?.playerName || currentChallengePlayerName || 'Player';
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
        <div class="challenge-race-progress" id="challenge-race-status">Updating challenge…</div>
    </div>
    <div class="game-card">
        <div class="stats" style="grid-template-columns: repeat(5, 1fr); gap: 12px;">
            <div class="stat"><div class="stat-value" id="attempts">0</div><div class="stat-label">Attempts</div></div>
            <div class="stat"><div class="stat-value" id="hints">3</div><div class="stat-label">Hints</div></div>
            <div class="stat"><div class="stat-value" id="best-match">0</div><div class="stat-label">Deepest Node</div></div>
            <div class="stat"><div class="stat-value" id="clades-revealed">0</div><div class="stat-label">Clades Shown</div></div>
            <div class="stat"><div class="stat-value" id="possible-specimens">-</div><div class="stat-label">Possible Answers</div></div>
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
                <button class="btn-hint btn-game-hint" onclick="useHint()" disabled title="Make 2 guesses before using a hint">Hint · 2 guesses</button>
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
            ? 'Waiting for another player to join'
            : `${total} players · ${completed} finished`;
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
            `<div class="standings-list">${rows || '<p>No players have joined yet.</p>'}</div>${data.requesterComplete ? '' : '<p class="standings-lock">Detailed scores appear after you finish, preventing outside information from influencing your game.</p>'}`
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

function buildResultMediaSlotMarkup() {
    return `
        <div class="victory-media-slot" aria-live="polite">
            <div class="victory-media-loading">Loading image…</div>
        </div>
    `;
}

async function hydrateResultMedia(panel, dinoName, mediaPromise) {
    const slot = panel.querySelector('.victory-media-slot');
    if (!slot) return;

    const media = await mediaPromise;
    if (!slot.isConnected) return;
    if (!media?.url) {
        slot.remove();
        return;
    }

    slot.innerHTML = buildResultMediaMarkup(dinoName, media);
    bindResultMedia(slot, dinoName, media);
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
    container.classList.add('tree-result-active');
    container.classList.remove('tree-review-active');

    let returnButton = container.querySelector('.tree-review-return');
    if (!returnButton) {
        returnButton = document.createElement('button');
        returnButton.type = 'button';
        returnButton.className = 'btn-hint btn-with-icon tree-review-return';
        returnButton.innerHTML = '<i class="ui-icon ui-icon-arrow-left" aria-hidden="true"></i><span>Back to Result</span>';
        returnButton.addEventListener('click', () => toggleResultTreeView(false));
        container.insertBefore(returnButton, container.firstChild);
    }

    panel.setAttribute('tabindex', '-1');
    panel.setAttribute('role', 'region');
    panel.setAttribute('aria-label', 'Challenge result');

    // The completed game becomes a stable result view instead of remaining
    // inside the draggable tree canvas.
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

function toggleResultTreeView(showTree = true) {
    const container = document.getElementById('tree-container');
    if (!container?.classList.contains('tree-result-active')) return;

    container.classList.toggle('tree-review-active', showTree);
    container.scrollTop = 0;
    container.scrollLeft = 0;

    const focusTarget = showTree
        ? container.querySelector('.tree-review-return')
        : container.querySelector('.victory');

    requestAnimationFrame(() => {
        if (showTree && typeof renderCurrentGameTree === 'function') {
            renderCurrentGameTree();
        }

        try {
            focusTarget?.focus({ preventScroll: true });
        } catch (_error) {
            focusTarget?.focus();
        }
        container.scrollIntoView({ behavior: 'smooth', block: 'start' });

        if (showTree) {
            requestAnimationFrame(() => {
                const victoryNodes = container.querySelectorAll('.tree-victory-node');
                const targetNode = victoryNodes[victoryNodes.length - 1];
                if (targetNode && typeof centerTreeElement === 'function') {
                    centerTreeElement(targetNode, 'auto');
                }
            });
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

    if (currentUserId && currentGameMode === 'daily') {
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
    } else if (currentGameMode === 'daily') {
        recordGuestDailyResult(false);
    }

    if (currentGameMode === 'challenge') {
        currentChallengeEliminated = false;
        await refreshCurrentChallengePlacement();
    }

    document.getElementById('dino-input').disabled = true;
    document.querySelector('.btn-guess').disabled = true;
    document.querySelector('.btn-game-hint').disabled = true;
    document.querySelector('.btn-giveup')?.setAttribute('disabled', true);

    const resultMediaPromise = loadResultMedia(targetDino.nome);

    const container = document.getElementById('tree-container');
    const v = document.createElement('div');
    v.className = 'victory victory--revealed';

    v.innerHTML = `
        <div class="victory-heading">
            <h2>ANSWER REVEALED</h2>
            <div class="victory-dino">${targetDino.nome}</div>
            <div class="victory-summary" aria-label="Game result summary">
                <span>${guesses.length} ${guesses.length === 1 ? 'attempt' : 'attempts'}</span>
                <span>Gave up</span>
            </div>
        </div>

        ${buildResultMediaSlotMarkup()}

        ${currentGameMode === 'challenge' && currentChallengePlacement ? `
        <div class="race-placement-card">
            <strong>#${currentChallengePlacement}</strong>
            <span>Your current race position. It becomes final when the race closes.</span>
        </div>` : ''}

        <div class="victory-actions">
            <button class="btn-hint victory-action-secondary" onclick="toggleResultTreeView(true)">
                View Tree
            </button>
            <button class="btn-hint victory-action-secondary" onclick="shareResult()" id="share-btn">
                Share Result
            </button>

            ${currentGameMode === 'challenge' ? `
            <button class="btn-hint victory-action-secondary" onclick="showChallengeStandings()">View Standings</button>
            <button class="btn-new-game" onclick="showFriendChallenges()">Return to Friend Challenges</button>` : `
            <button class="btn-new-game" onclick="${isPracticeMode ? 'showPracticeMode()' : 'showDifficultySelection()'}">
                ${isPracticeMode ? 'Play Again' : 'Return to Level Selection'}
            </button>`}
        </div>
    `;

    container.insertBefore(v, container.firstChild);
    hydrateResultMedia(v, targetDino.nome, resultMediaPromise);
    isGiveUpMode = true;
    gameWon = true;
    setTreeAnimationMode('reveal');
    redrawGameTree();
    updateCladeInfo();
    revealResultPanel(container, v);
}

function buildVictoryStreakMarkup(streakData, milestone) {
    if (!streakData) return '';
    if (milestone) {
        return `
            <div class="streak-celebration streak-celebration--milestone">
            <div class="streak-milestone-title">◆ ${milestone} DAY MILESTONE! ◆</div>
            <div class="streak-current">Current Streak: ${streakData.current} days</div>
            <div class="streak-best">Best: ${streakData.best} days</div>
            </div>
        `;
    }

    return `
        <div class="streak-celebration">
        <div class="streak-title">◆ ${streakData.current} Day Streak</div>
        <div class="streak-best">Best: ${streakData.best} days</div>
        </div>
    `;
}

function buildVictoryAchievementsMarkup(achievementIds) {
    if (!Array.isArray(achievementIds) || achievementIds.length === 0) return '';

    const rows = achievementIds.map(id => {
        const definition = ACHIEVEMENT_DEFINITIONS.find(achievement => achievement.id === id);
        if (!definition) return '';
        return `
            <div class="victory-achievement-item">
                <span class="achievement-medal" aria-hidden="true"></span>
                <span>${definition.name}</span>
            </div>
        `;
    }).join('');

    return `
        <section class="victory-achievements" aria-label="Achievements unlocked">
            <div class="victory-achievements-kicker">New achievement${achievementIds.length === 1 ? '' : 's'}</div>
            <div class="victory-achievements-list">${rows}</div>
        </section>
    `;
}

function buildVictoryDiscoveryMarkup(discovery) {
    if (!discovery) return '';

    const count = Math.max(Number(discovery.discoveryCount) || 1, 1);
    const isNew = discovery.isFirstDiscovery === true;
    const countLabel = count === 1
        ? 'Now part of your collection'
        : `Discovered ${count} times`;

    return `
        <section class="victory-discovery${isNew ? ' victory-discovery--new' : ''}"
                 aria-label="${isNew ? 'New Museum discovery' : 'Museum discovery updated'}">
            <span class="victory-discovery-mark" aria-hidden="true">
                <svg class="victory-discovery-footprint" viewBox="0 0 512 512" focusable="false">
                    <path
                        d="M511.517 370.284c-5.971-20.294-92.954-22.906-113.159-18.866-20.216 4.041-76.79 2.68-88.913-9.443-12.124-12.124 4.041-32.329 16.175-44.451 12.124-12.124 38.389-50.524 46.472-58.606 8.082-8.082 50.166-50.378 65.325-74.758 30.988-49.843 41.437-98.68 25.262-114.845-16.164-16.164-65.003-5.727-114.834 25.262-24.38 15.17-66.688 57.244-74.769 65.325-8.083 8.083-46.472 34.36-58.606 46.483-12.122 12.124-32.328 28.287-44.451 16.164-12.122-12.124-13.474-68.698-9.433-88.902 4.041-20.205 1.419-107.187-18.866-113.16C121.438-5.483 83.798 44.94 68.984 87.37c-8.685 24.871-82.852 141.446-66.688 226.319 16.175 84.872 31 127.982 49.507 146.503 18.519 18.519 61.642 33.343 146.503 49.507 84.872 16.164 201.448-57.991 226.319-66.676 42.431-14.825 92.853-52.466 86.892-72.739z"
                        transform="translate(76 76) scale(.703125)"
                    />
                </svg>
            </span>
            <div class="victory-discovery-copy">
                <div class="victory-discovery-kicker">${isNew ? 'New Museum Discovery' : 'Museum Record Updated'}</div>
                <strong data-victory-discovery-primary>${isNew ? `${targetDino.nome} has been added to the Museum` : countLabel}</strong>
                <span data-victory-discovery-secondary>${isNew ? countLabel : 'Its entry remains available in your collection.'}</span>
            </div>
            <button class="btn-hint victory-discovery-action" type="button" data-open-victory-museum>
                View Entry
            </button>
        </section>
    `;
}

async function hydrateVictoryDiscoveryCount(panel, name, isNew) {
    try {
        const records = await getDiscoveryRecords();
        const count = Math.max(Number(records[name.toLowerCase()]?.count) || 1, 1);
        const countLabel = count === 1
            ? 'Now part of your collection'
            : `Discovered ${count} times`;
        const primary = panel.querySelector('[data-victory-discovery-primary]');
        const secondary = panel.querySelector('[data-victory-discovery-secondary]');

        if (isNew) {
            if (secondary) secondary.textContent = countLabel;
        } else if (primary) {
            primary.textContent = countLabel;
        }
    } catch (error) {
        console.warn('Could not refresh the Museum discovery count:', error);
    }
}

async function openVictoryMuseumEntry(name, button = null) {
    if (!name) return;

    const originalText = button?.textContent;
    if (button) {
        button.disabled = true;
        button.textContent = 'Opening…';
    }

    try {
        museumDiscoveryRecords = await getDiscoveryRecords();
        if (!Array.isArray(fullDatabase) || !fullDatabase.some(dino => dino.nome === name)) {
            const catalog = await callGameApi('catalog');
            fullDatabase = catalog.dinosaurs || [];
        }
        await showMuseumEntry(name);
    } catch (error) {
        console.error('Could not open Museum entry from victory:', error);
        await customAlert('Could Not Open Museum Entry', error.message);
    } finally {
        if (button && document.body.contains(button)) {
            button.disabled = false;
            button.textContent = originalText || 'View Entry';
        }
    }
}

async function persistVictoryResult() {
    let streakData = null;
    let milestone = null;
    let newlyUnlockedAchievements = [];

    if (currentUserId && currentGameMode === 'daily') {
        await clearGameProgress(selectedDifficulty);
        streakData = await updateStreak();
        milestone = checkStreakMilestone(streakData.current);
        newlyUnlockedAchievements = await updateStatsAfterGame(true, guesses.length, selectedDifficulty);
        await markDailyChallengeCompleted(selectedDifficulty);
    } else if (currentGameMode === 'daily') {
        newlyUnlockedAchievements = recordGuestDailyResult(true);
    }

    if (currentGameMode === 'challenge') {
        currentChallengeEliminated = false;
        await refreshCurrentChallengePlacement();
    }

    if (!currentUserId && currentGameMode !== 'daily') {
        newlyUnlockedAchievements = recordGuestGameResult(true);
    }

    if (currentUserId) {
        try {
            const synchronization = await syncAccountAchievements();
            newlyUnlockedAchievements = [...new Set([
                ...newlyUnlockedAchievements,
                ...synchronization.newlyUnlocked
            ])];
        } catch (error) {
            console.error('Account achievement synchronization failed:', error);
        }
    }

    return {
        streakData,
        milestone,
        placement: currentChallengePlacement,
        newlyUnlockedAchievements
    };
}

async function hydrateVictoryMetadata(panel, persistencePromise) {
    const status = panel.querySelector('.victory-save-status');
    try {
        const result = await persistencePromise;
        const streakSlot = panel.querySelector('.victory-streak-slot');
        const placementSlot = panel.querySelector('.challenge-placement-slot');
        const achievementSlot = panel.querySelector('.victory-achievements-slot');
        if (streakSlot) streakSlot.innerHTML = buildVictoryStreakMarkup(result.streakData, result.milestone);
        if (achievementSlot) {
            const markup = buildVictoryAchievementsMarkup(result.newlyUnlockedAchievements);
            if (markup) achievementSlot.innerHTML = markup;
            else achievementSlot.remove();
        }
        if (placementSlot && result.placement) {
            placementSlot.innerHTML = `
                <div class="race-placement-card">
                    <strong>#${result.placement}</strong>
                    <span>Your finishing position</span>
                </div>
            `;
        }
        status?.remove();
    } catch (error) {
        console.error('Victory result persistence error:', error);
        if (status) status.textContent = 'Result saved locally; online statistics will retry on your next visit.';
    } finally {
        panel.querySelectorAll('[data-victory-action]').forEach(button => {
            button.disabled = false;
        });
    }
}

async function showVictory() {
    const discovery = registerDiscovery(targetDino.nome, currentMuseumProof);
    const persistencePromise = persistVictoryResult();

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
        <div class="victory-mode-note">
            Practice Mode - Statistics not recorded
        </div>
        `;
    }
    if (currentGameMode === 'challenge') {
        modeHTML = `
        <div class="challenge-result-note">
            Friend Challenge <strong>${escapeChallengeHtml(currentChallengeCode)}</strong> - Daily statistics not affected
        </div>
        <div class="challenge-placement-slot"></div>`;
    }

    const streakHTML = currentUser && currentGameMode === 'daily'
        ? '<div class="victory-streak-slot"></div>'
        : '';
    const achievementHTML = '<div class="victory-achievements-slot"></div>';

    const resultMediaPromise = loadResultMedia(targetDino.nome);
        v.innerHTML = `
            ${modeHTML}
            <div class="victory-heading">
                <h2>CHALLENGE COMPLETE</h2>
                <div class="victory-dino">${targetDino.nome}</div>
                <div class="victory-summary" aria-label="Game result summary">
                    <span>${guesses.length} ${guesses.length === 1 ? 'attempt' : 'attempts'}</span>
                    <span>${revealedClades.size} ${revealedClades.size === 1 ? 'clade' : 'clades'} revealed</span>
                </div>
            </div>

            ${buildResultMediaSlotMarkup()}

            ${buildVictoryDiscoveryMarkup(discovery)}

            ${streakHTML}
            ${achievementHTML}
            <div class="victory-save-status">Saving result…</div>

            <div class="victory-actions">
                <button class="btn-hint victory-action-secondary" onclick="toggleResultTreeView(true)">
                    View Tree
                </button>
                <button class="btn-hint victory-action-secondary" data-victory-action onclick="shareResult()" id="share-btn" disabled>
                    Share Result
                </button>
                ${currentGameMode === 'challenge' ? `
                <button class="btn-hint victory-action-secondary" data-victory-action onclick="showChallengeStandings()" disabled>View Standings</button>
                <button class="btn-new-game" data-victory-action disabled onclick="showFriendChallenges()">Return to Friend Challenges</button>` : `
                <button class="btn-new-game" data-victory-action disabled onclick="${isPracticeMode ? 'showPracticeMode()' : 'showDifficultySelection()'}">
                    ${isPracticeMode ? 'Play Again' : 'Return to Level Selection'}
                </button>`}
            </div>
        `;

    container.insertBefore(v, container.firstChild);
    const museumButton = v.querySelector('[data-open-victory-museum]');
    museumButton?.addEventListener('click', () => openVictoryMuseumEntry(targetDino.nome, museumButton));
    hydrateVictoryDiscoveryCount(v, targetDino.nome, discovery.isFirstDiscovery);
    hydrateResultMedia(v, targetDino.nome, resultMediaPromise);
    hydrateVictoryMetadata(v, persistencePromise);
    revealResultPanel(container, v);
}

function getShareResultData() {
    const diffNames = {
        'muito_facil': 'Level I',
        'facil': 'Level II',
        'normal': 'Level III',
        'dificil': 'Level IV',
        'muito_dificil': 'Level V'
    };
    const actualGuesses = guesses.filter(guess => guess.isHint !== true);
    const blocks = actualGuesses.map(guess => {
        const percentage = Number(guess.proximity?.percentage || 0);
        if (percentage === 100) return '🟩';
        if (percentage >= 75) return '🟨';
        if (percentage >= 50) return '🟧';
        if (percentage >= 25) return '🟥';
        return '⬛';
    });
    const rows = [];
    for (let index = 0; index < blocks.length; index += 8) {
        rows.push(blocks.slice(index, index + 8).join(''));
    }

    const appUrl = new URL(window.location.href);
    appUrl.hash = '';
    appUrl.search = '';
    if (currentGameMode === 'challenge' && currentChallengeCode) {
        appUrl.searchParams.set('challenge', currentChallengeCode);
    }

    const modeLabel = currentGameMode === 'challenge'
        ? `Friend Challenge ${currentChallengeCode}`
        : isPracticeMode ? 'Practice' : 'Daily Challenge';
    const hintCount = Array.isArray(hintHistory) ? hintHistory.length : 0;
    const attemptLabel = `${actualGuesses.length} ${actualGuesses.length === 1 ? 'guess' : 'guesses'}`;
    const hintLabel = `${hintCount} ${hintCount === 1 ? 'hint' : 'hints'}`;
    const placementLabel = currentGameMode === 'challenge' && currentChallengePlacement
        ? ` • #${currentChallengePlacement} place`
        : '';

    return {
        title: `Phylosaur - ${diffNames[selectedDifficulty] || 'Challenge'}`,
        modeLabel,
        difficultyLabel: diffNames[selectedDifficulty] || 'Challenge',
        dateLabel: getCurrentDateFormatted(),
        rows,
        attemptLabel,
        hintLabel,
        placementLabel,
        outcomeLabel: isGiveUpMode ? `Answer revealed after ${attemptLabel}` : `Solved in ${attemptLabel}`,
        url: appUrl.toString()
    };
}

function buildShareResultText(result = getShareResultData()) {
    return [
        `PHYLOSAUR 🦖`,
        `${result.modeLabel} • ${result.difficultyLabel}`,
        result.dateLabel,
        '',
        result.rows.join('\n'),
        '',
        `${result.outcomeLabel} • ${result.hintLabel}${result.placementLabel}`,
        '',
        result.url
    ].join('\n');
}

function escapeShareResultHtml(value) {
    return String(value).replace(/[&<>]/g, character => ({
        '&': '&amp;', '<': '&lt;', '>': '&gt;'
    })[character]);
}

async function copyShareResult(text) {
    if (navigator.clipboard?.writeText) {
        await navigator.clipboard.writeText(text);
        return;
    }

    const textarea = document.createElement('textarea');
    textarea.value = text;
    textarea.setAttribute('readonly', '');
    textarea.style.position = 'fixed';
    textarea.style.opacity = '0';
    document.body.appendChild(textarea);
    textarea.select();
    const copied = document.execCommand('copy');
    textarea.remove();
    if (!copied) throw new Error('Copy command was not accepted.');
}

function showShareButtonFeedback(message) {
    const button = document.getElementById('share-btn');
    if (!button) return;
    const original = button.textContent;
    button.textContent = message;
    button.disabled = true;
    setTimeout(() => {
        if (!button.isConnected) return;
        button.textContent = original;
        button.disabled = false;
    }, 2000);
}

async function shareResult() {
    const result = getShareResultData();
    const text = buildShareResultText(result);
    const action = await showModal({
        title: 'Share Your Result',
        message: `
            <div class="share-result-intro">Spoiler-free: dinosaur and clade names stay hidden.</div>
            <pre class="share-result-preview">${escapeShareResultHtml(text)}</pre>
            <div class="share-result-legend">
                <span>⬛ distant</span><span>🟥 warmer</span><span>🟧 close</span><span>🟨 very close</span><span>🟩 solved</span>
            </div>
        `,
        buttons: [
            { text: navigator.share ? 'Share' : 'Copy Result', value: 'share', primary: true },
            ...(navigator.share ? [{ text: 'Copy', value: 'copy', primary: false }] : []),
            { text: 'Cancel', value: 'cancel', primary: false }
        ],
        closeOnOverlay: true
    });

    if (action === 'cancel' || action === null) return;

    try {
        if (action === 'share' && navigator.share) {
            await navigator.share({ title: result.title, text });
            showShareButtonFeedback('Shared!');
            return;
        }

        await copyShareResult(text);
        showShareButtonFeedback('Copied to Clipboard!');
    } catch (error) {
        if (error?.name === 'AbortError') return;
        console.error('Result sharing error:', error);
        await customAlert('Could Not Share', 'Your result could not be copied. Please try again.');
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