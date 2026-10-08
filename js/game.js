// ═══════════════════════════════════════════════
// GAME INITIALIZATION AND MAIN LOGIC
// ═══════════════════════════════════════════════
function renderGameSessionShell(loadingMessage, { contextLabel = '', tutorial = false } = {}) {
    const context = contextLabel
        ? `<p class="game-mode-context">${escapeChallengeHtml(contextLabel)}</p>`
        : '';

    return `
    <div class="game-card game-session-card">
        ${context}
        <div class="stats game-session-stats">
            <div class="game-session-stat-main">
                <div class="stat">
                    <div class="stat-value" id="attempts">0</div>
                    <div class="stat-label">${t('game.attempts')}</div>
                </div>
                <div class="stat">
                    <div class="stat-value" id="hints">3</div>
                    <div class="stat-label">${t('game.hints')}</div>
                </div>
                <div class="stat">
                    <div class="stat-value" id="best-match">0</div>
                    <div class="stat-label">${t('game.deepestNode')}</div>
                </div>
                <div class="stat">
                    <div class="stat-value" id="clades-revealed">0</div>
                    <div class="stat-label">${t('game.cladesShown')}</div>
                </div>
            </div>
            <div class="game-session-stat-sidebar">
                <div class="stat">
                    <div class="stat-value" id="possible-specimens">-</div>
                    <div class="stat-label">${t('game.possibleAnswers')}</div>
                </div>
            </div>
        </div>

        ${tutorial ? `
        <aside id="tutorial-coach" class="tutorial-coach" aria-live="polite" aria-atomic="true"></aside>
        ` : ''}

        <div class="game-session-layout">
            <div class="game-session-main">
                <div class="input-section">
                    <div class="guess-primary-row">
                        <div class="guess-field">
                            <label class="visually-hidden" for="dino-input">${t('game.guessLabel')}</label>
                            <input type="text" id="dino-input" placeholder="${t('game.guessPlaceholder')}" autocomplete="off" autocorrect="off" autocapitalize="none" spellcheck="false" writingsuggestions="false" enterkeyhint="go" />
                            <div id="suggestions"></div>
                        </div>
                        <button class="btn-guess" onclick="makeGuess()">${t('game.submit')}</button>
                    </div>
                    <div class="guess-secondary-row">
                        <button class="btn-hint btn-game-hint" onclick="useHint()" disabled
                                title="${t('game.hintUnlock', { count: 2, unit: t('game.guessMany') })}">
                            ${t('game.hintCount', { count: 2, unit: t('game.guessMany') })}
                        </button>
                        ${tutorial ? '' : `<button class="btn-giveup" onclick="giveUp()">${t('game.giveUp')}</button>`}
                    </div>
                </div>

                <div id="tree-container">
                    <div id="tree-scroll-wrapper">
                        ${renderAppState(loadingMessage, { compact: true })}
                    </div>
                </div>
                <div id="clade-info"></div>
            </div>
            <div class="game-session-sidebar">
                <div id="tree-toolbar-host"></div>
                <aside id="guess-history" aria-label="${t('history.title')}"></aside>
            </div>
        </div>
    </div>`;
}

const TUTORIAL_TARGET_NAME = 'Velociraptor';
const TUTORIAL_GUESS_SEQUENCE = Object.freeze([
    'Triceratops', 'Brachiosaurus', 'Tyrannosaurus', 'Velociraptor'
]);
const TUTORIAL_DINOSAURS = Object.freeze([
    {
        nome: 'Triceratops',
        linhagem: ['Dinosauria', 'Ornithischia', 'Genasauria', 'Cerapoda', 'Marginocephalia',
            'Ceratopsia', 'Neoceratopsia', 'Coronosauria', 'Ceratopsidae', 'Chasmosaurinae']
    },
    {
        nome: 'Brachiosaurus',
        linhagem: ['Dinosauria', 'Saurischia', 'Eusaurischia', 'Sauropodomorpha', 'Bagualosauria',
            'Plateosauria', 'Massopoda', 'Anchisauria', 'Sauropodiformes', 'Sauropoda',
            'Eusauropoda', 'Neosauropoda', 'Macronaria', 'Camarasauromorpha',
            'Titanosauriformes', 'Brachiosauridae']
    },
    {
        nome: 'Tyrannosaurus',
        linhagem: ['Dinosauria', 'Saurischia', 'Eusaurischia', 'Theropoda', 'Neotheropoda',
            'Averostra', 'Tetanurae', 'Orionides', 'Avetheropoda', 'Coelurosauria',
            'Tyrannoraptora', 'Tyrannosauroidea', 'Tyrannosauridae', 'Tyrannosaurinae',
            'Tyrannosaurini']
    },
    {
        nome: 'Velociraptor',
        linhagem: ['Dinosauria', 'Saurischia', 'Eusaurischia', 'Theropoda', 'Neotheropoda',
            'Averostra', 'Tetanurae', 'Orionides', 'Avetheropoda', 'Coelurosauria',
            'Tyrannoraptora', 'Maniraptoriformes', 'Maniraptora', 'Pennaraptora', 'Paraves',
            'Dromaeosauridae', 'Eudromaeosauria', 'Velociraptorinae']
    }
]);
let tutorialStage = 0;
let tutorialReady = false;

function commonTutorialLineagePrefix(first, second) {
    let matches = 0;
    while (matches < first.length && matches < second.length && first[matches] === second[matches]) {
        matches++;
    }
    return matches;
}

function buildTutorialTreeSnapshot() {
    const targetLineage = targetDino.linhagem;
    const visibleClades = new Set(['Dinosauria', ...revealedClades]);
    guesses.forEach(guess => {
        if (guess.proximity.lastCommonClade) visibleClades.add(guess.proximity.lastCommonClade);
    });

    const nodes = new Map();
    nodes.set('Dinosauria', {
        depth: 0, children: [], type: 'root', lineageIndex: 0,
        isHinted: revealedClades.has('Dinosauria')
    });
    [...visibleClades]
        .filter(clade => clade !== 'Dinosauria')
        .sort((first, second) => targetLineage.indexOf(first) - targetLineage.indexOf(second))
        .forEach(clade => {
            const lineageIndex = targetLineage.indexOf(clade);
            if (lineageIndex < 0) return;
            let parent = 'Dinosauria';
            for (let index = lineageIndex - 1; index >= 0; index--) {
                if (nodes.has(targetLineage[index])) {
                    parent = targetLineage[index];
                    break;
                }
            }
            nodes.set(clade, {
                depth: nodes.get(parent).depth + 1,
                children: [],
                type: 'internal',
                lineageIndex,
                isHinted: revealedClades.has(clade)
            });
            nodes.get(parent).children.push(clade);
        });

    const leaves = guesses.map((guess, index) => ({
        name: guess.dino.nome + '__tutorial_' + index,
        displayName: guess.dino.nome,
        parentNode: guess.proximity.lastCommonClade || 'Dinosauria',
        isTarget: gameWon && guess.dino.nome === TUTORIAL_TARGET_NAME,
        isGiveUp: false,
        isHint: false
    }));
    if (!gameWon) {
        let mysteryParent = 'Dinosauria';
        visibleClades.forEach(clade => {
            if (targetLineage.indexOf(clade) > targetLineage.indexOf(mysteryParent)) mysteryParent = clade;
        });
        leaves.push({
            name: '?__leaf_tutorial',
            displayName: '?',
            parentNode: mysteryParent,
            isTarget: true,
            isGiveUp: false,
            isHint: false
        });
    }
    return {
        root: 'Dinosauria',
        nodes: [...nodes].map(([name, node]) => ({ name, ...node })),
        leaves
    };
}

function getTutorialCoachStep() {
    return [
        { anchor: '.input-section', title: t('tutorial.coachFirstTitle'), copy: t('tutorial.coachFirstCopy') },
        { anchor: '#tree-container', title: t('tutorial.coachTreeTitle'), copy: t('tutorial.coachTreeCopy') },
        { anchor: '.guess-secondary-row', title: t('tutorial.coachHintTitle'), copy: t('tutorial.coachHintCopy') },
        { anchor: '.input-section', title: t('tutorial.coachCloserTitle'), copy: t('tutorial.coachCloserCopy') },
        { anchor: '.input-section', title: t('tutorial.coachSolveTitle'), copy: t('tutorial.coachSolveCopy') }
    ][Math.min(tutorialStage, 4)];
}

function renderTutorialCoach() {
    const coach = document.getElementById('tutorial-coach');
    if (!coach || currentGameMode !== 'tutorial' || gameWon) return;
    document.querySelectorAll('.tutorial-focus').forEach(element => element.classList.remove('tutorial-focus'));
    const step = getTutorialCoachStep();
    const anchor = document.querySelector(step.anchor);
    anchor?.classList.add('tutorial-focus');
    const progress = Array.from({ length: 5 }, (_, index) =>
        '<span class="' + (index < tutorialStage ? 'is-complete' :
            index === tutorialStage ? 'is-current' : '') + '"></span>'
    ).join('');
    coach.innerHTML =
        '<div class="tutorial-coach-header">' +
            '<div class="tutorial-coach-progress">' +
                '<span class="tutorial-coach-label">' + t('tutorial.context') + '</span>' +
                '<span class="tutorial-coach-count">' +
                t('tutorial.progress', { current: tutorialStage + 1, total: 5 }) +
                '</span>' +
            '</div>' +
            '<div class="tutorial-coach-actions">' +
                '<button type="button" class="btn-hint tutorial-coach-action" onclick="startTutorialGame()">' +
                    t('tutorial.restart') +
                '</button>' +
                '<button type="button" class="btn-hint tutorial-coach-action tutorial-coach-skip" onclick="skipTutorialGame()">' +
                    t('tutorial.skip') +
                '</button>' +
            '</div>' +
        '</div>' +
        '<div class="tutorial-coach-body">' +
            '<h2>' + step.title + '</h2>' +
            '<p>' + step.copy + '</p>' +
            '<div class="tutorial-coach-track" aria-hidden="true">' + progress + '</div>' +
        '</div>';
}

function updateTutorialDisplay(animationMode = 'default', focusKey = null) {
    const snapshot = buildTutorialTreeSnapshot();
    window.currentTreeSnapshot = snapshot;
    setTreeAnimationMode(animationMode, focusKey);
    const bestMatch = guesses.length
        ? Math.max(...guesses.map(guess => guess.proximity.matches))
        : 0;
    document.getElementById('attempts').textContent = String(guesses.length);
    document.getElementById('hints').textContent = String(hintsRemaining);
    document.getElementById('best-match').textContent = String(bestMatch);
    document.getElementById('clades-revealed').textContent = String(revealedClades.size);
    document.getElementById('possible-specimens').textContent = String(database.length);
    updateHintButtonState();
    renderTreeSnapshot(snapshot);
    updateGuessHistory();
    const info = document.getElementById('clade-info');
    if (info) info.innerHTML = '';
    renderTutorialCoach();
}

async function startTutorialGame() {
    setAppRoute('/tutorial');
    setHeaderControls('game');
    currentGameMode = 'tutorial';
    tutorialReady = false;
    const appContent = document.getElementById('app-content');
    appContent.innerHTML = renderGameSessionShell(t('tutorial.loading'), {
        contextLabel: t('tutorial.context'),
        tutorial: true
    });
    const wrapper = document.getElementById('tree-scroll-wrapper');
    try {
        const catalog = TUTORIAL_DINOSAURS;
        if (document.getElementById('tree-scroll-wrapper') !== wrapper ||
            getCurrentAppRoute() !== '/tutorial') return;
        const required = new Set([TUTORIAL_TARGET_NAME, ...TUTORIAL_GUESS_SEQUENCE]);
        database = catalog.filter(dino => required.has(dino.nome));
        targetDino = database.find(dino => dino.nome === TUTORIAL_TARGET_NAME);
        if (!targetDino || database.length !== required.size) throw new Error(t('tutorial.loadError'));

        currentGameMode = 'tutorial';
        isPracticeMode = false;
        selectedDifficulty = 'muito_facil';
        gameSessionId = 'tutorial-local';
        tutorialStage = 0;
        guesses = [];
        guessedNames = new Set();
        hintsRemaining = 1;
        hintHistory = [];
        revealedClades = new Set();
        guessesSinceLastHint = 0;
        gameWon = false;
        gameRequestPending = false;
        challengeRaceClosing = false;
        currentTargetDepth = targetDino.linhagem.length;
        serverPossibleSpecimens = database.length;
        window.collapsedClades.clear();
        window.currentTreeSnapshot = null;
        if (typeof resetTreeAnimationState === 'function') resetTreeAnimationState();
        tutorialReady = true;
        updateTutorialDisplay();
        initializeAutocomplete();
        document.getElementById('dino-input')?.focus();
    } catch (error) {
        if (wrapper && document.getElementById('tree-scroll-wrapper') === wrapper) {
            wrapper.innerHTML = renderAppState(t('tutorial.loadError'), {
                type: 'error',
                detail: error.message,
                compact: true
            });
        }
    }
}

async function makeTutorialGuess() {
    if (!tutorialReady || gameWon) return;
    const input = document.getElementById('dino-input');
    if (tutorialStage === 2) {
        await customAlert(t('tutorial.tryTitle'), t('tutorial.followCoach'));
        document.querySelector('.btn-game-hint')?.focus();
        return;
    }
    const guessName = input?.value.trim() || '';
    const sequenceIndex = tutorialStage > 2 ? tutorialStage - 1 : tutorialStage;
    const expectedName = TUTORIAL_GUESS_SEQUENCE[sequenceIndex];
    const available = database.find(dino => dino.nome.toLowerCase() === guessName.toLowerCase());
    if (!available || available.nome !== expectedName) {
        await customAlert(t('tutorial.tryTitle'), t('tutorial.tryExpected', { name: expectedName }));
        input?.focus();
        return;
    }

    const matches = commonTutorialLineagePrefix(available.linhagem, targetDino.linhagem);
    const won = available.nome === TUTORIAL_TARGET_NAME;
    guesses.push({
        dino: { nome: available.nome },
        proximity: {
            matches,
            percentage: Math.round((matches / currentTargetDepth) * 100),
            lastCommonClade: targetDino.linhagem[matches - 1] || null,
            divergenceDepth: matches
        },
        isHint: false
    });
    guessedNames.add(available.nome.toLowerCase());
    guessesSinceLastHint++;
    gameWon = won;
    tutorialStage++;
    if (input) input.value = '';
    const suggestions = document.getElementById('suggestions');
    if (suggestions) suggestions.style.display = 'none';
    updateTutorialDisplay(won ? 'victory' : 'guess', 'display:' + available.nome);
    if (won) showTutorialCompletion();
    else input?.focus();
}

async function useTutorialHint() {
    if (!tutorialReady || gameWon) return;
    if (tutorialStage !== 2) {
        await customAlert(t('game.hintUnavailable'), t('tutorial.followCoach'));
        return;
    }
    const cladeName = 'Theropoda';
    revealedClades.add(cladeName);
    hintHistory.push({
        cladeName,
        depth: targetDino.linhagem.indexOf(cladeName) + 1
    });
    hintsRemaining = 0;
    guessesSinceLastHint = 0;
    tutorialStage = 3;
    updateTutorialDisplay('hint', 'node:' + cladeName);
    await customAlert(
        t('game.hint'),
        t('game.nextClade') + '<br><br><strong>' + cladeName + '</strong>'
    );
    document.getElementById('dino-input')?.focus();
}

function showTutorialCompletion() {
    tutorialReady = false;
    markFirstRunTutorialComplete();
    document.querySelectorAll('.tutorial-focus').forEach(element => element.classList.remove('tutorial-focus'));
    document.getElementById('tutorial-coach')?.remove();
    const input = document.getElementById('dino-input');
    if (input) input.disabled = true;
    document.querySelector('.btn-guess')?.setAttribute('disabled', true);
    document.querySelector('.btn-game-hint')?.setAttribute('disabled', true);
    document.querySelector('.btn-giveup')?.setAttribute('disabled', true);
    const resultMediaPromise = loadResultMedia(TUTORIAL_TARGET_NAME);
    const panel = document.createElement('section');
    panel.className = 'victory tutorial-completion';
    panel.setAttribute('aria-live', 'polite');
    panel.innerHTML =
        '<div class="victory-heading">' +
            '<span class="tutorial-coach-progress">' + t('tutorial.completeKicker') + '</span>' +
            '<h2>' + t('tutorial.completeTitle') + '</h2>' +
            '<div class="victory-dino"><em>' + TUTORIAL_TARGET_NAME + '</em></div>' +
            '<p>' + t('tutorial.completeCopy') + '</p>' +
        '</div>' +
        buildResultMediaSlotMarkup() +
        '<div class="victory-actions tutorial-completion-actions">' +
            '<button class="btn-hint" onclick="toggleResultTreeView(true)">' +
                t('game.viewTree') +
            '</button>' +
            '<button class="btn-new-game" onclick="startDailyChallenge(\'muito_facil\')">' +
                t('tutorial.playLevelOne') +
            '</button>' +
            '<button class="btn-hint" onclick="startTutorialGame()">' + t('tutorial.repeat') + '</button>' +
            '<button class="btn-hint" onclick="navigateToAppRoute(\'/\')">' + t('tutorial.return') + '</button>' +
        '</div>';
    const container = document.getElementById('tree-container');
    if (!container) return;
    container.insertBefore(panel, container.firstChild);
    hydrateResultMedia(panel, TUTORIAL_TARGET_NAME, resultMediaPromise);
    revealResultPanel(container, panel);
}

function skipTutorialGame() {
    tutorialReady = false;
    markFirstRunTutorialComplete();
    navigateToAppRoute('/');
}

async function startPracticeChallenge(difficulty, { restoreExisting = false } = {}) {
    setAppRoute(`/game/practice/${difficulty}`);
    setHeaderControls('practice');
    currentGameMode = 'practice';
    selectedDifficulty = difficulty;
    
    const appContent = document.getElementById('app-content');
    
    appContent.innerHTML = renderGameSessionShell(t('game.loadingPractice'), {
        contextLabel: t('game.practiceContext')
    });
    await loadPracticeDatabase(difficulty, !restoreExisting, restoreExisting);
}

async function startDailyChallenge(difficulty, { restoreExisting = false } = {}) {
    setAppRoute(`/game/daily/${difficulty}`);
    setHeaderControls('game');
    isPracticeMode = false;
    currentGameMode = 'daily';
    selectedDifficulty = difficulty;
    const appContent = document.getElementById('app-content');
    
    appContent.innerHTML = renderGameSessionShell(t('game.loadingDaily'));

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
    currentAccountProgress = null;
    currentGameMode = 'challenge';
    isPracticeMode = false;
    selectedDifficulty = data.difficulty;
    currentChallengeCode = data.challenge?.code || currentChallengeCode;
    if (currentChallengeCode) setAppRoute(`/challenge/${currentChallengeCode}`);
    currentChallengePlayerName = data.challenge?.playerName || currentChallengePlayerName || t('common.player');
    currentChallengeCreatorName = data.challenge?.creatorName || currentChallengeCreatorName;
    currentChallengePlacement = data.challenge?.placement ?? null;
    currentChallengeTotalPlayers = Number(data.challenge?.totalPlayers || 0);
    currentChallengeEliminated = Boolean(data.challenge?.eliminated);
    challengeRaceClosing = false;

    const appContent = document.getElementById('app-content');
    const challengeBanner = `
    <div class="challenge-banner">
        <div>
            <span>${t('friends.challenge')}</span>
            <strong>${escapeChallengeHtml(currentChallengeCode)}</strong>
        </div>
        <div class="challenge-banner-actions">
            <button class="btn-hint btn-header" onclick="copyChallengeCode()">${t('friends.copyCode')}</button>
            <button class="btn-hint btn-header" onclick="showChallengeStandings()">${t('friends.standings')}</button>
        </div>
        <div class="challenge-race-progress" id="challenge-race-status">${t('friends.updating')}</div>
    </div>`;
    appContent.innerHTML = challengeBanner + renderGameSessionShell(t('game.loadingFriend'));

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

function getChallengeSessionGuard() {
    const generation = challengeStatusPollGeneration;
    const sessionId = gameSessionId;
    const code = currentChallengeCode;
    const ownerId = currentUserId;
    const wrapper = document.getElementById('tree-scroll-wrapper');
    return () => Boolean(wrapper) && document.getElementById('tree-scroll-wrapper') === wrapper
        && currentGameMode === 'challenge' && gameSessionId === sessionId
        && currentChallengeCode === code && currentUserId === ownerId
        && challengeStatusPollGeneration === generation;
}

function stopChallengeStatusPolling() {
    challengeStatusPollGeneration++;
    challengeStatusPollInFlight = false;
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
            ? t('friends.waiting')
            : t('friends.raceProgress', { total, completed });
    }
}

async function handleChallengeRaceClosure(statusData) {
    if (challengeRaceClosing || currentGameMode !== 'challenge') return;
    challengeRaceClosing = true;
    stopChallengeStatusPolling();
    const isCurrentRequest = getChallengeSessionGuard();
    updateChallengeRaceStatus(statusData);
    currentChallengeEliminated = true;

    try {
        const state = await callGameApi('state', { sessionId: gameSessionId });
        if (!isCurrentRequest()) return;
        applyServerGamePayload(state);
        currentChallengeEliminated = true;
        currentChallengePlacement = statusData.race?.requesterPlacement ||
            state.challenge?.placement || currentChallengeTotalPlayers;
        setTreeAnimationMode('reveal');
        updateServerGameDisplay(state);
        await showRestoredServerCompletion(state);
    } catch (error) {
        if (!isCurrentRequest()) return;
        await customAlert(t('friends.raceComplete'), t('friends.raceCompleteCopy'));
    }
}

async function pollChallengeRaceStatus() {
    if (currentGameMode !== 'challenge' || !currentChallengeCode || !gameSessionId || challengeRaceClosing) {
        stopChallengeStatusPolling();
        return;
    }
    if (document.hidden || challengeStatusPollInFlight) return;
    const generation = challengeStatusPollGeneration;
    const isCurrentRequest = getChallengeSessionGuard();
    if (!isCurrentRequest()) {
        stopChallengeStatusPolling();
        return;
    }

    challengeStatusPollInFlight = true;
    try {
        const data = await callGameApi('challenge_status', {
            code: currentChallengeCode,
            sessionId: gameSessionId
        });
        if (!isCurrentRequest()) return;
        updateChallengeRaceStatus(data);
        if (data.race?.closedRequester) {
            await handleChallengeRaceClosure(data);
        } else if (data.requesterComplete) {
            stopChallengeStatusPolling();
        }
    } catch (error) {
        console.warn('Challenge race status unavailable:', error);
    } finally {
        if (generation === challengeStatusPollGeneration) challengeStatusPollInFlight = false;
    }
}

function startChallengeStatusPolling() {
    stopChallengeStatusPolling();
    void pollChallengeRaceStatus();
    challengeStatusPollTimer = setInterval(() => void pollChallengeRaceStatus(), 4000);
}

document.addEventListener('visibilitychange', () => {
    if (!document.hidden && challengeStatusPollTimer) void pollChallengeRaceStatus();
});

async function refreshCurrentChallengePlacement() {
    if (currentGameMode !== 'challenge' || !currentChallengeCode || !gameSessionId) return null;
    stopChallengeStatusPolling();
    const isCurrentRequest = getChallengeSessionGuard();
    try {
        const data = await callGameApi('challenge_status', {
            code: currentChallengeCode,
            sessionId: gameSessionId
        });
        if (!isCurrentRequest()) return null;
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
        await customAlert(t('friends.codeCopied'), `<strong class="challenge-code-inline">${escapeChallengeHtml(currentChallengeCode)}</strong><br><br>${t('friends.sendCode')}`);
    } catch (error) {
        await customAlert(t('friends.codeTitle'), `<strong class="challenge-code-inline">${escapeChallengeHtml(currentChallengeCode)}</strong>`);
    }
}

let challengeStandingsGeneration = 0;

async function showChallengeStandings() {
    if (!currentChallengeCode || !gameSessionId) return;
    const generation = ++challengeStandingsGeneration;
    const code = currentChallengeCode;
    const isCurrentSession = getGameSessionGuard();
    const isCurrentRequest = () => generation === challengeStandingsGeneration
        && currentChallengeCode === code && isCurrentSession();
    if (!isCurrentRequest()) return;
    try {
        const data = await callGameApi('challenge_status', {
            code,
            sessionId: gameSessionId
        });
        if (!isCurrentRequest()) return;
        updateChallengeRaceStatus(data);
        if (data.race?.closedRequester) {
            await handleChallengeRaceClosure(data);
            return;
        }
        const rows = data.participants.map((participant, index) => {
            const status = participant.status === 'solved'
                ? t('friends.finished')
                : participant.status === 'eliminated'
                ? t('friends.raceClosed')
                : participant.status === 'gave_up'
                ? t('result.gaveUp')
                : t('friends.playing');
            const details = data.requesterComplete
                ? t('friends.standingDetails', {
                    attempts: participant.attempts,
                    hints: participant.hintsUsed,
                    playing: participant.status === 'playing' ? ` · ${t('friends.playing')}` : ''
                })
                : status;
            return `<div class="standing-row ${participant.isYou ? 'is-you' : ''}">
                <span class="standing-rank">${data.requesterComplete ? (participant.placement ? `#${participant.placement}` : '…') : '◆'}</span>
                <span class="standing-name">${escapeChallengeHtml(participant.name)}${participant.isYou ? ` ${t('friends.you')}` : ''}</span>
                <span class="standing-result">${escapeChallengeHtml(details)}</span>
            </div>`;
        }).join('');
        await customAlert(
            t('friends.challengeWithCode', { code: escapeChallengeHtml(code) }),
            `<div class="standings-list">${rows || `<p>${t('friends.noPlayers')}</p>`}</div>${data.requesterComplete ? '' : `<p class="standings-lock">${t('friends.scoresHidden')}</p>`}`
        );
    } catch (error) {
        if (!isCurrentRequest()) return;
        await customAlert(t('friends.standingsError'), escapeHtml(error.message));
    }
}

function redrawGameTree() {
    renderCurrentGameTree();
}

function getGameSessionGuard() {
    const sessionId = gameSessionId;
    const ownerId = currentUserId;
    const mode = currentGameMode;
    const wrapper = document.getElementById('tree-scroll-wrapper');
    return () => Boolean(wrapper) && document.getElementById('tree-scroll-wrapper') === wrapper
        && gameSessionId === sessionId && currentUserId === ownerId && currentGameMode === mode;
}

function beginGameAction() {
    if (gameWon || gameRequestPending || !gameSessionId || challengeRaceClosing
        || document.querySelector('[data-app-modal="true"]')) return null;
    const isCurrentSession = getGameSessionGuard();
    if (!isCurrentSession()) return null;
    const generation = ++gameActionGeneration;
    setGuessRequestPending(true);
    return () => generation === gameActionGeneration && isCurrentSession();
}

async function refreshConflictedGameSession(error, isCurrentRequest) {
    if (error?.status !== 409 || error.data?.code !== "SESSION_CONFLICT") return false;
    if (!isCurrentRequest()) return true;
    try {
        const state = await callGameApi("state", { sessionId: gameSessionId });
        if (!isCurrentRequest()) return true;
        applyServerGamePayload(state);
        setTreeAnimationMode(state.complete ? "reveal" : "default");
        updateServerGameDisplay(state);
        if (state.complete) {
            await showRestoredServerCompletion(state);
            if (!isCurrentRequest()) return true;
        }
        await customAlert(t("game.sessionUpdatedTitle"), t("game.sessionUpdatedCopy"));
    } catch (refreshError) {
        if (!isCurrentRequest()) return true;
        await customAlert(t("game.sessionUpdatedTitle"), escapeHtml(refreshError.message));
    }
    return true;
}

function setGuessRequestPending(pending) {
    gameRequestPending = pending;
    const unavailable = pending || gameWon || challengeRaceClosing;
    const input = document.getElementById('dino-input');
    const button = document.querySelector('.btn-guess');

    if (button) {
        button.textContent = pending ? t('game.analyzing') : t('game.submit');
        button.disabled = unavailable;
    }
    if (input) input.disabled = unavailable;
    const giveUpButton = document.querySelector('.btn-giveup');
    if (giveUpButton) giveUpButton.disabled = unavailable;
    updateHintButtonState();

    if (!pending && !gameWon && input) {
        requestAnimationFrame(() => {
            if (document.getElementById('dino-input') === input && !gameRequestPending
                && !gameWon && !challengeRaceClosing
                && !document.querySelector('[data-app-modal="true"]')) input.focus();
        });
    }
}

async function explainPracticeGuess(guessIndex) {
    if (currentGameMode !== 'practice') return;
    const guess = guesses[Number(guessIndex)];
    if (!guess || guess.dino?.nome === targetDino?.nome) return;

    const explanation = guess.explanation || {};
    const guessedDino = database.find(dinosaur => dinosaur.nome === guess.dino.nome);
    const lineage = Array.isArray(guessedDino?.linhagem) ? guessedDino.linhagem : [];
    const matches = Math.max(0, Number(guess.proximity?.matches) || 0);
    const lineageDepth = Number(explanation.guessLineageDepth) || lineage.length;
    const genus = escapeHtml(guess.dino.nome);
    let message;

    if (matches === 0) {
        message = t('history.explainNoSharedClade', { genus });
    } else if (lineageDepth > 0 && matches >= lineageDepth) {
        message = t('history.explainFullPath', { genus, matches: lineageDepth });
    } else {
        const sharedClade = guess.proximity?.lastCommonClade || lineage[matches - 1];
        const nextGuessClade = explanation.nextGuessClade || lineage[matches];
        if (sharedClade && nextGuessClade) {
            message = t('history.explainDivergence', {
                genus,
                sharedClade: escapeHtml(sharedClade),
                nextGuessClade: escapeHtml(nextGuessClade)
            });
        } else {
            message = t('history.explainUnavailable', { genus });
        }
    }

    await customAlert(t('history.explainTitle'), message);
}

async function makeGuess() {
    if (gameWon || document.querySelector('[data-app-modal="true"]')) return;
    if (currentGameMode === 'tutorial') return makeTutorialGuess();
    await makeServerGuess();
}

async function makeServerGuess() {
    if (gameWon || gameRequestPending || !gameSessionId || challengeRaceClosing
        || document.querySelector('[data-app-modal="true"]')) return;

    const input = document.getElementById('dino-input');
    const guessName = input?.value.trim() || '';

    if (!guessName) {
        await customAlert(t('game.enterNameTitle'), t('game.enterNameCopy'));
        return;
    }

    const available = database.find(
        dinosaur => dinosaur.nome.toLowerCase() === guessName.toLowerCase()
    );
    if (!available) {
        await customAlert(t('game.notFoundTitle'), t('game.notFoundCopy'));
        return;
    }

    if (guessedNames.has(available.nome.toLowerCase())) {
        await customAlert(t('game.alreadyGuessedTitle'), t('game.alreadyGuessedCopy'));
        return;
    }

    const isCurrentRequest = beginGameAction();
    if (!isCurrentRequest) return;

    let data;
    try {
        data = await callGameApi('guess', {
            sessionId: gameSessionId,
            guess: available.nome
        });
    } catch (error) {
        if (!isCurrentRequest()) return;
        try {
            if (gameWon || challengeRaceClosing) return;
            if (await refreshConflictedGameSession(error, isCurrentRequest)) return;
            if (!isCurrentRequest()) return;
            await customAlert(t('game.guessRejected'), escapeHtml(error.message));
        } finally {
            if (isCurrentRequest()) setGuessRequestPending(false);
        }
        return;
    }

    try {
        if (!isCurrentRequest() || gameWon || challengeRaceClosing) return;
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
        if (!isCurrentRequest()) return;
        console.error('Error displaying accepted guess:', error);
        await customAlert(
            t('game.displayErrorTitle'),
            t('game.displayErrorCopy')
        );
    } finally {
        if (isCurrentRequest()) setGuessRequestPending(false);
    }
}

async function useHint() {
    if (currentGameMode === 'tutorial') return useTutorialHint();
    await useServerHint();
}

async function useServerHint() {
    const isCurrentRequest = beginGameAction();
    if (!isCurrentRequest) return;

    try {
        const data = await callGameApi('hint', { sessionId: gameSessionId });
        if (!isCurrentRequest() || gameWon || challengeRaceClosing) return;
        applyServerGamePayload(data);
        guessesSinceLastHint = 0;
        const isCladeHint = Boolean(data.hint?.cladeName);
        setTreeAnimationMode(
            isCladeHint ? 'hint' : 'default',
            isCladeHint ? `node:${data.hint.cladeName}` : null
        );
        updateServerGameDisplay(data);

        if (isCladeHint) {
            await customAlert(
                t('game.hint'),
                `${t('game.nextClade')}<br><br><strong style="color:var(--color-primary); font-size:1.2em;">${escapeHtml(data.hint.cladeName)}</strong>`
            );
            if (!isCurrentRequest()) return;
            await updateCladeInfo();
        } else {
            const nameHintKeys = {
                initial: 'game.nameStartsWith',
                length: 'game.nameHasLetters',
                ending: 'game.nameEndsWith'
            };
            const nameHint = data.hint?.clue && nameHintKeys[data.hint.clue]
                ? t(nameHintKeys[data.hint.clue], { value: data.hint.value })
                : t('game.nameHintFallback');
            await customAlert(
                t('game.nameHintTitle'),
                `<strong style="color:var(--color-primary); font-size:1.2em;">${escapeHtml(nameHint)}</strong>`
            );
        }
    } catch (error) {
        if (!isCurrentRequest() || gameWon || challengeRaceClosing) return;
        if (await refreshConflictedGameSession(error, isCurrentRequest)) return;
        if (!isCurrentRequest()) return;
        const missing = Number(error.data?.guessesRequired || 0);
        const message = missing > 0
            ? t('game.hintWait', {
                count: `<strong>${missing}</strong>`,
                unit: t(missing === 1 ? 'game.guessOne' : 'game.guessMany')
            })
            : escapeHtml(error.message);
        await customAlert(t('game.hintUnavailable'), message);
    } finally {
        if (isCurrentRequest()) setGuessRequestPending(false);
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
            : `${t('media.imageSource')} <a href="${sourcePage}" target="_blank" rel="noopener">Wikimedia Commons</a>`;

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
            <button class="victory-media-image-button" type="button"
                aria-label="${escapeHtml(t('media.imageViewer', { name: dinoName }))}">
                <img class="victory-media-image" src="${escapeHtml(media.url)}" alt="${escapeHtml(dinoName)}">
            </button>
            <div class="victory-media-credit">${media.credit}</div>
        </div>
    `;
}

function buildResultMediaSlotMarkup() {
    return `
        <div class="victory-media-slot" aria-live="polite">
            <div class="victory-media-loading">${t('museum.loadingImage')}</div>
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

    panel.querySelector('.victory-media-image-button')?.addEventListener('click', () => {
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
        returnButton.innerHTML = `<i class="ui-icon ui-icon-arrow-left" aria-hidden="true"></i><span>${t('result.back')}</span>`;
        returnButton.addEventListener('click', () => toggleResultTreeView(false));
        container.insertBefore(returnButton, container.firstChild);
    }

    panel.setAttribute('tabindex', '-1');
    panel.setAttribute('role', 'region');
    panel.setAttribute('aria-label', t('result.challengeResult'));

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
    const isCurrentRequest = beginGameAction();
    if (!isCurrentRequest) return;

    try {
        const confirm = await customConfirm(
            t('game.giveUpTitle'),
            t('game.giveUpCopy'),
            t('game.giveUp'),
            t('game.keepTrying')
        );

        if (!isCurrentRequest() || gameWon || challengeRaceClosing || confirm !== 'true') return;

        try {
            const data = await callGameApi('give_up', { sessionId: gameSessionId });
            if (!isCurrentRequest() || gameWon || challengeRaceClosing) return;
            applyServerGamePayload(data);
        } catch (error) {
            if (!isCurrentRequest() || gameWon || challengeRaceClosing) return;
            if (await refreshConflictedGameSession(error, isCurrentRequest)) return;
            if (!isCurrentRequest()) return;
            await customAlert(t('game.giveUpError'), escapeHtml(error.message));
            return;
        }

        if (currentUserId && currentGameMode === 'daily') {
            try {
                await ensureDailyAccountProgress();
                if (!isCurrentRequest()) return;
                await syncAccountAchievements();
            } catch (error) {
                console.error('Daily progress finalization failed:', error);
            }
        } else if (currentGameMode === 'daily') {
            recordGuestDailyResult(false);
        }
        if (!isCurrentRequest()) return;

        if (currentGameMode === 'challenge') {
            currentChallengeEliminated = false;
            await refreshCurrentChallengePlacement();
        }
        if (!isCurrentRequest()) return;

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
                <h2>${t('result.answerRevealedTitle')}</h2>
                <div class="victory-dino">${targetDino.nome}</div>
                <div class="victory-summary" aria-label="${t('game.resultSummary')}">
                    <span>${guesses.length} ${t(guesses.length === 1 ? 'game.attemptOne' : 'game.attemptMany')}</span>
                    <span>${t('result.gaveUp')}</span>
                </div>
            </div>

            ${buildResultMediaSlotMarkup()}

            ${currentGameMode === 'challenge' && currentChallengePlacement ? `
            <div class="race-placement-card">
                <strong>#${currentChallengePlacement}</strong>
                <span>${t('friends.currentRacePosition')}</span>
            </div>` : ''}

            <div class="victory-actions">
                <button class="btn-hint victory-action-secondary" onclick="toggleResultTreeView(true)">
                    ${t('game.viewTree')}
                </button>
                <button class="btn-hint victory-action-secondary" onclick="shareResult()" id="share-btn">
                    ${t('result.share')}
                </button>

                ${currentGameMode === 'challenge' ? `
                <button class="btn-hint victory-action-secondary" onclick="showChallengeStandings()">${t('game.viewStandings')}</button>
                <button class="btn-new-game" onclick="showFriendChallenges()">${t('game.returnFriends')}</button>` : `
                <button class="btn-new-game" onclick="${isPracticeMode ? 'showPracticeMode()' : 'showDifficultySelection()'}">
                    ${isPracticeMode ? t('game.playAgain') : t('game.returnLevels')}
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
    } finally {
        if (isCurrentRequest()) setGuessRequestPending(false);
    }
}

function buildVictoryStreakMarkup(streakData, milestone) {
    if (!streakData) return '';
    if (milestone) {
        return `
            <div class="streak-celebration streak-celebration--milestone">
            <div class="streak-milestone-title">◆ ${t('result.dayMilestone', { count: milestone })} ◆</div>
            <div class="streak-current">${t('result.currentStreakDays', { count: streakData.current })}</div>
            <div class="streak-best">${t('result.bestStreakDays', { count: streakData.best })}</div>
            </div>
        `;
    }

    return `
        <div class="streak-celebration">
        <div class="streak-title">◆ ${t('result.dayStreak', { count: streakData.current })}</div>
        <div class="streak-best">${t('result.bestStreakDays', { count: streakData.best })}</div>
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
                <span>${getAchievementName(definition)}</span>
            </div>
        `;
    }).join('');

    return `
        <section class="victory-achievements" aria-label="${t('result.achievementsUnlocked')}">
            <div class="victory-achievements-kicker">${t(achievementIds.length === 1
                ? 'result.newAchievementOne'
                : 'result.newAchievementMany')}</div>
            <div class="victory-achievements-list">${rows}</div>
        </section>
    `;
}

function buildVictoryDiscoveryMarkup(discovery) {
    if (!discovery) return '';

    const count = Math.max(Number(discovery.discoveryCount) || 1, 1);
    const isNew = discovery.isFirstDiscovery === true;
    const countLabel = currentUserId
        ? t('common.loading')
        : count === 1
            ? t('result.nowInCollection')
            : t('result.discoveredTimes', { count });

    return `
        <section class="victory-discovery${isNew ? ' victory-discovery--new' : ''}"
                 aria-label="${t(isNew ? 'result.newMuseumDiscovery' : 'result.museumDiscoveryUpdated')}">
            <span class="victory-discovery-mark" aria-hidden="true">
                <svg class="victory-discovery-footprint" viewBox="0 0 512 512" focusable="false">
                    <path
                        d="M511.517 370.284c-5.971-20.294-92.954-22.906-113.159-18.866-20.216 4.041-76.79 2.68-88.913-9.443-12.124-12.124 4.041-32.329 16.175-44.451 12.124-12.124 38.389-50.524 46.472-58.606 8.082-8.082 50.166-50.378 65.325-74.758 30.988-49.843 41.437-98.68 25.262-114.845-16.164-16.164-65.003-5.727-114.834 25.262-24.38 15.17-66.688 57.244-74.769 65.325-8.083 8.083-46.472 34.36-58.606 46.483-12.122 12.124-32.328 28.287-44.451 16.164-12.122-12.124-13.474-68.698-9.433-88.902 4.041-20.205 1.419-107.187-18.866-113.16C121.438-5.483 83.798 44.94 68.984 87.37c-8.685 24.871-82.852 141.446-66.688 226.319 16.175 84.872 31 127.982 49.507 146.503 18.519 18.519 61.642 33.343 146.503 49.507 84.872 16.164 201.448-57.991 226.319-66.676 42.431-14.825 92.853-52.466 86.892-72.739z"
                        transform="translate(76 76) scale(.703125)"
                    />
                </svg>
            </span>
            <div class="victory-discovery-copy">
                <div class="victory-discovery-kicker">${t(isNew ? 'result.newMuseumDiscovery' : 'result.museumRecordUpdated')}</div>
                <strong data-victory-discovery-primary>${isNew ? t('result.addedToMuseum', { name: targetDino.nome }) : countLabel}</strong>
                <span data-victory-discovery-secondary>${isNew ? countLabel : t('result.entryRemains')}</span>
            </div>
            <button class="btn-hint victory-discovery-action" type="button" data-open-victory-museum>
                ${t('museum.viewEntry')}
            </button>
        </section>
    `;
}

async function hydrateVictoryDiscoveryCount(panel, name, isNew, fallbackCount = 1) {
    const discoveryOwnerId = currentUserId;
    const publishCount = count => {
        if (!panel.isConnected || currentUserId !== discoveryOwnerId) return;
        const countLabel = count === 1
            ? t('result.nowInCollection')
            : t('result.discoveredTimes', { count });
        const primary = panel.querySelector('[data-victory-discovery-primary]');
        const secondary = panel.querySelector('[data-victory-discovery-secondary]');

        if (isNew) {
            if (secondary) secondary.textContent = countLabel;
        } else if (primary) {
            primary.textContent = countLabel;
        }
    };

    try {
        const records = await getDiscoveryRecords();
        const count = Math.max(Number(records[name.toLowerCase()]?.count) || 1, 1);
        publishCount(count);
    } catch (error) {
        console.warn('Could not refresh the Museum discovery count:', error);
        publishCount(Math.max(Number(fallbackCount) || 1, 1));
    }
}

async function openVictoryMuseumEntry(name, button = null) {
    if (!name) return;

    const originalText = button?.textContent;
    if (button) {
        button.disabled = true;
        button.textContent = t('museum.opening');
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
        await customAlert(t('museum.openError'), escapeHtml(error.message));
    } finally {
        if (button && document.body.contains(button)) {
            button.disabled = false;
            button.textContent = originalText || t('museum.viewEntry');
        }
    }
}

async function persistVictoryResult() {
    const isCurrentRequest = getGameSessionGuard();
    let streakData = null;
    let milestone = null;
    let newlyUnlockedAchievements = [];

    if (currentUserId && currentGameMode === 'daily') {
        streakData = await ensureDailyAccountProgress();
        if (!isCurrentRequest()) return {};
        milestone = streakData ? checkStreakMilestone(streakData.current) : null;
    } else if (currentGameMode === 'daily') {
        newlyUnlockedAchievements = recordGuestDailyResult(true);
    }

    if (currentGameMode === 'challenge') {
        currentChallengeEliminated = false;
        await refreshCurrentChallengePlacement();
        if (!isCurrentRequest()) return {};
    }

    if (!currentUserId && currentGameMode !== 'daily') {
        newlyUnlockedAchievements = recordGuestGameResult(true);
    }

    if (currentUserId) {
        try {
            const synchronization = await syncAccountAchievements();
            if (!isCurrentRequest()) return {};
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
    const isCurrentRequest = getGameSessionGuard();
    const status = panel.querySelector('.victory-save-status');
    try {
        const result = await persistencePromise;
        if (!isCurrentRequest() || !panel.isConnected) return;
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
                    <span>${t('friends.finishingPosition')}</span>
                </div>
            `;
        }
        status?.remove();
    } catch (error) {
        if (!isCurrentRequest() || !panel.isConnected) return;
        console.error('Victory result persistence error:', error);
        if (status) status.textContent = t('result.savedStatsRetry');
    } finally {
        if (isCurrentRequest() && panel.isConnected) panel.querySelectorAll('[data-victory-action]').forEach(button => {
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
            ${t('game.practiceNoStats')}
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
                <h2>${t('result.completeTitle')}</h2>
                <div class="victory-dino">${targetDino.nome}</div>
                <div class="victory-summary" aria-label="${t('game.resultSummary')}">
                    <span>${guesses.length} ${t(guesses.length === 1 ? 'game.attemptOne' : 'game.attemptMany')}</span>
                    <span>${revealedClades.size} ${t(revealedClades.size === 1 ? 'game.cladeOne' : 'game.cladeMany')} ${t('game.revealed')}</span>
                </div>
            </div>

            ${buildResultMediaSlotMarkup()}

            ${buildVictoryDiscoveryMarkup(discovery)}

            ${streakHTML}
            ${achievementHTML}
            <div class="victory-save-status">${t('result.saving')}</div>

            <div class="victory-actions">
                <button class="btn-hint victory-action-secondary" onclick="toggleResultTreeView(true)">
                    ${t('game.viewTree')}
                </button>
                <button class="btn-hint victory-action-secondary" data-victory-action onclick="shareResult()" id="share-btn" disabled>
                    ${t('result.share')}
                </button>
                ${currentGameMode === 'challenge' ? `
                <button class="btn-hint victory-action-secondary" data-victory-action onclick="showChallengeStandings()" disabled>${t('game.viewStandings')}</button>
                <button class="btn-new-game" data-victory-action disabled onclick="showFriendChallenges()">${t('game.returnFriends')}</button>` : `
                <button class="btn-new-game" data-victory-action disabled onclick="${isPracticeMode ? 'showPracticeMode()' : 'showDifficultySelection()'}">
                    ${isPracticeMode ? t('game.playAgain') : t('game.returnLevels')}
                </button>`}
            </div>
        `;

    container.insertBefore(v, container.firstChild);
    const museumButton = v.querySelector('[data-open-victory-museum]');
    museumButton?.addEventListener('click', () => openVictoryMuseumEntry(targetDino.nome, museumButton));
    hydrateVictoryDiscoveryCount(v, targetDino.nome, discovery.isFirstDiscovery, discovery.discoveryCount);
    hydrateResultMedia(v, targetDino.nome, resultMediaPromise);
    hydrateVictoryMetadata(v, persistencePromise);
    revealResultPanel(container, v);
}

function getShareResultData() {
    const diffNames = {
        'muito_facil': t('level.name1'),
        'facil': t('level.name2'),
        'normal': t('level.name3'),
        'dificil': t('level.name4'),
        'muito_dificil': t('level.name5')
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
        ? t('result.friendChallengeCode', { code: currentChallengeCode })
        : isPracticeMode ? t('analytics.practice') : t('home.daily');
    const hintCount = Array.isArray(hintHistory) ? hintHistory.length : 0;
    const attemptLabel = `${actualGuesses.length} ${t(actualGuesses.length === 1 ? 'game.guessOne' : 'game.guessMany')}`;
    const hintLabel = `${hintCount} ${t(hintCount === 1 ? 'game.hintOne' : 'game.hintMany')}`;
    const placementLabel = currentGameMode === 'challenge' && currentChallengePlacement
        ? t('result.placement', { placement: currentChallengePlacement })
        : '';

    return {
        title: `Phylosaur - ${diffNames[selectedDifficulty] || t('friends.challenge')}`,
        modeLabel,
        difficultyLabel: diffNames[selectedDifficulty] || t('friends.challenge'),
        dateLabel: getCurrentDateFormatted(),
        rows,
        attemptLabel,
        hintLabel,
        placementLabel,
        outcomeLabel: isGiveUpMode
            ? t('result.answerAfter', { attempts: attemptLabel })
            : t('result.solvedIn', { attempts: attemptLabel }),
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
        title: t('result.shareTitle'),
        message: `
            <div class="share-result-intro">${t('result.shareSpoilerFree')}</div>
            <pre class="share-result-preview">${escapeShareResultHtml(text)}</pre>
            <div class="share-result-legend">
                <span>⬛ ${t('result.distanceDistant')}</span><span>🟥 ${t('result.distanceWarmer')}</span><span>🟧 ${t('result.distanceClose')}</span><span>🟨 ${t('result.distanceVeryClose')}</span><span>🟩 ${t('result.distanceSolved')}</span>
            </div>
        `,
        buttons: [
            { text: navigator.share ? t('result.shareAction') : t('result.copyResult'), value: 'share', primary: true },
            ...(navigator.share ? [{ text: t('result.copy'), value: 'copy', primary: false }] : []),
            { text: t('common.cancel'), value: 'cancel', primary: false }
        ],
        closeOnOverlay: true
    });

    if (action === 'cancel' || action === null) return;

    try {
        if (action === 'share' && navigator.share) {
            await navigator.share({ title: result.title, text });
            showShareButtonFeedback(t('result.shared'));
            return;
        }

        await copyShareResult(text);
        showShareButtonFeedback(t('result.copied'));
    } catch (error) {
        if (error?.name === 'AbortError') return;
        console.error('Result sharing error:', error);
        await customAlert(t('result.shareErrorTitle'), t('result.shareErrorCopy'));
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
    return formatPhylosaurDate(today, options);
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