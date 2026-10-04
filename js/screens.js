// ═══════════════════════════════════════════════
// SCREENS AND INTERFACE LOGIC
// ═══════════════════════════════════════════════
function escapeChallengeHtml(value) {
    return escapeHtml(value);
}

async function showDifficultySelection() {
    setAppRoute('/');
    if (typeof stopChallengeStatusPolling === 'function') stopChallengeStatusPolling();
    const completionStatus = getImmediateDailyCompletionStatus();
    setHeaderControls('difficulty');
    const appContent = document.getElementById('app-content');
    
    appContent.innerHTML = `            
        <div class="game-card difficulty-home">
        <h2 class="difficulty-home-title screen-title">${t('home.daily')}</h2>
        <p class="difficulty-home-date">
            ${getCurrentDateFormatted()} — ${t('home.chooseLevel')}
        </p>
        <p class="difficulty-home-countdown">
        ${t('home.nextDaily')} <span id="countdown-timer">--:--:--</span>
        </p>

        <div class="difficulty-home-levels">
            ${generateDifficultyButton('muito_facil', t('level.1'), 'I', '', completionStatus.muito_facil)}
            ${generateDifficultyButton('facil', t('level.2'), 'II', '', completionStatus.facil)}
            ${generateDifficultyButton('normal', t('level.3'), 'III', '', completionStatus.normal)}
            ${generateDifficultyButton('dificil', t('level.4'), 'IV', '', completionStatus.dificil)}
            ${generateDifficultyButton('muito_dificil', t('level.5'), 'V', '', completionStatus.muito_dificil)}
        </div>

        <div class="difficulty-home-actions-wrap">
            <div class="difficulty-home-actions">
            <button class="btn-hint btn-large btn-menu-action" onclick="showHowToPlay()">
                ${t('home.howToPlay')}
            </button>
            <button class="btn-hint btn-large btn-menu-action" onclick="showPracticeMode()">
                ${t('home.practice')}
            </button>
            <button class="btn-hint btn-friends btn-large btn-menu-action" onclick="showFriendChallenges()">
                ${t('home.friends')}
            </button>
            </div>
        </div>
    `;
    startCountdown();
    void refreshDifficultySelectionAccountState();
    focusAppScreenHeading();
}

function emptyDailyCompletionStatus() {
    return { muito_facil: false, facil: false, normal: false, dificil: false, muito_dificil: false };
}

function getImmediateDailyCompletionStatus() {
    if (!currentUserId) return emptyDailyCompletionStatus();
    const cacheKey = `${currentUserId}:${getTodayString()}`;
    return dailyCompletionCache?.key === cacheKey
        ? { ...dailyCompletionCache.status }
        : emptyDailyCompletionStatus();
}

function updateDifficultyCompletionStatus(status) {
    Object.entries(status || {}).forEach(([difficulty, completed]) => {
        const button = document.querySelector(`.difficulty-btn[data-difficulty="${difficulty}"]`);
        if (!button) return;
        button.classList.toggle('difficulty-completed', completed === true);
        let mark = button.querySelector('.difficulty-completion-mark');
        if (completed && !mark) {
            mark = document.createElement('span');
            mark.className = 'difficulty-completion-mark';
            mark.setAttribute('aria-label', t('common.completed'));
            mark.textContent = '✓';
            button.appendChild(mark);
        } else if (!completed) {
            mark?.remove();
        }
    });
}

async function refreshDifficultySelectionAccountState() {
    if (getCurrentAppRoute() !== '/') return;
    const [completionStatus] = await Promise.all([
        getDailyCompletionStatus(),
        initializeAnalyticsAccess()
    ]);
    if (getCurrentAppRoute() !== '/' || !document.querySelector('.difficulty-btn[data-difficulty]')) return;
    updateDifficultyCompletionStatus(completionStatus);
    setHeaderControls('difficulty');
}

function showFriendChallenges(prefilledCode = '') {
    setAppRoute('/friends');
    if (typeof stopChallengeStatusPolling === 'function') stopChallengeStatusPolling();
    setHeaderControls('friends');
    const appContent = document.getElementById('app-content');
    const suggestedName = currentChallengePlayerName || currentUser || '';
    const code = String(prefilledCode || '').toUpperCase().replace(/[^A-Z0-9]/g, '').slice(0, 6);

    appContent.innerHTML = `
    <div class="game-card friends-hub">
        <div class="friends-heading">
            <div class="friends-kicker">${t('friends.private')}</div>
            <h2 class="screen-title">${t('home.friends')}</h2>
            <p>${t('friends.intro')}</p>
        </div>

        <div class="friends-grid">
            <section class="friend-panel">
                <h3>${t('friends.create')}</h3>
                <label class="friend-label" for="challenge-create-name">${t('friends.yourName')}</label>
                <input class="friend-input" id="challenge-create-name" maxlength="24" value="${escapeChallengeHtml(suggestedName)}" placeholder="${t('friends.playerName')}">

                <label class="friend-label" for="challenge-difficulty">${t('friends.level')}</label>
                <select class="friend-input" id="challenge-difficulty">
                    <option value="muito_facil">${t('level.name1')}</option>
                    <option value="facil">${t('level.name2')}</option>
                    <option value="normal" selected>${t('level.name3')}</option>
                    <option value="dificil">${t('level.name4')}</option>
                    <option value="muito_dificil">${t('level.name5')}</option>
                </select>

                <button class="btn-guess friend-action" id="create-challenge-btn" onclick="createFriendChallenge()">${t('friends.createCode')}</button>
            </section>

            <div class="friends-divider" aria-hidden="true"><span>${t('friends.or')}</span></div>

            <section class="friend-panel">
                <h3>${t('friends.join')}</h3>
                <label class="friend-label" for="challenge-join-name">${t('friends.yourName')}</label>
                <input class="friend-input" id="challenge-join-name" maxlength="24" value="${escapeChallengeHtml(suggestedName)}" placeholder="${t('friends.playerName')}">

                <label class="friend-label" for="challenge-code">${t('friends.challengeCode')}</label>
                <input class="friend-input challenge-code-input" id="challenge-code" maxlength="6" value="${escapeChallengeHtml(code)}" placeholder="RAPTOR" autocomplete="off" autocapitalize="characters" spellcheck="false"
                       oninput="this.value=this.value.toUpperCase().replace(/[^A-Z0-9]/g,'').slice(0,6)"
                       onkeydown="if(event.key==='Enter') joinFriendChallenge()">

                <button class="btn-hint friend-action" id="join-challenge-btn" onclick="joinFriendChallenge()">${t('friends.enter')}</button>
            </section>
        </div>

        <p class="friends-note">${t('friends.note')}</p>
    </div>`;

    if (code) document.getElementById('challenge-join-name')?.focus();
    else focusAppScreenHeading();
}

function beginFriendChallengeRequest() {
    const appContent = document.getElementById('app-content');
    const createButton = document.getElementById('create-challenge-btn');
    const joinButton = document.getElementById('join-challenge-btn');
    if (!appContent || !createButton || !joinButton || createButton.disabled || joinButton.disabled) return null;
    const hub = appContent.firstElementChild;
    const ownerId = currentUserId;
    const isCurrent = () => appContent.firstElementChild === hub && currentUserId === ownerId;
    createButton.disabled = true;
    joinButton.disabled = true;
    return {
        isCurrent,
        finish() {
            if (!isCurrent()) return;
            createButton.disabled = false;
            joinButton.disabled = false;
            createButton.textContent = t('friends.createCode');
            joinButton.textContent = t('friends.enter');
        }
    };
}

async function createFriendChallenge() {
    const nameInput = document.getElementById('challenge-create-name');
    const difficultyInput = document.getElementById('challenge-difficulty');
    const button = document.getElementById('create-challenge-btn');
    const playerName = nameInput?.value.trim().slice(0, 24) || t('common.player');
    const request = beginFriendChallengeRequest();
    if (!request) return;

    button.textContent = t('friends.creating');
    try {
        const data = await callGameApi('create_challenge', {
            difficulty: difficultyInput.value,
            playerName
        });
        if (!request.isCurrent()) return;
        localStorage.setItem(getChallengeSessionStorageKey(data.challenge.code), data.sessionId);
        await startFriendChallengeFromPayload(data);
    } catch (error) {
        if (!request.isCurrent()) return;
        await customAlert(t('friends.createError'), escapeHtml(error.message));
    } finally {
        request.finish();
    }
}

async function joinFriendChallenge() {
    const nameInput = document.getElementById('challenge-join-name');
    const codeInput = document.getElementById('challenge-code');
    const button = document.getElementById('join-challenge-btn');
    if (!button || button.disabled) return;
    const playerName = nameInput?.value.trim().slice(0, 24) || t('common.player');
    const code = codeInput?.value.toUpperCase().replace(/[^A-Z0-9]/g, '') || '';

    if (code.length !== 6) {
        await customAlert(t('friends.invalidCode'), t('friends.invalidCodeCopy'));
        if (document.getElementById('challenge-code') === codeInput) codeInput?.focus();
        return;
    }

    const request = beginFriendChallengeRequest();
    if (!request) return;
    button.textContent = t('friends.entering');
    try {
        await loadChallengeDatabase(code, playerName, { isCurrentRequest: request.isCurrent });
    } catch (error) {
        if (!request.isCurrent()) return;
        await customAlert(t('friends.enterError'), escapeHtml(error.message));
    } finally {
        request.finish();
    }
}

function showPracticeMode() {
    setAppRoute('/practice');
    setHeaderControls('practice-menu');
    const appContent = document.getElementById('app-content');
    
    appContent.innerHTML = `
    <div class="game-card difficulty-home practice-menu">
        <h2 class="difficulty-home-title screen-title">${t('home.practice')}</h2>
        <p class="difficulty-home-date">
        ${t('home.practiceCopy')}
        </p>

        <div class="difficulty-home-levels">
        ${generatePracticeDifficultyButton('muito_facil', t('level.1'), 'I')}
        ${generatePracticeDifficultyButton('facil', t('level.2'), 'II')}
        ${generatePracticeDifficultyButton('normal', t('level.3'), 'III')}
        ${generatePracticeDifficultyButton('dificil', t('level.4'), 'IV')}
        ${generatePracticeDifficultyButton('muito_dificil', t('level.5'), 'V')}
        </div>
    </div>
    `;
    focusAppScreenHeading();
}

function generatePracticeDifficultyButton(difficulty, name, level) {
    const tierCount = {'muito_facil': 1, 'facil': 2, 'normal': 3, 'dificil': 4, 'muito_dificil': 5}[difficulty];
    let tiers = '';
    for (let i = 1; i <= 5; i++) {
    tiers += `<span class="tier-indicator ${i <= tierCount ? 'filled' : ''}"></span>`;
    }
    
    return `
    <button class="difficulty-btn difficulty-${DIFFICULTY_MAP[difficulty]}" 
            onclick="startPracticeChallenge('${difficulty}')">
        <div class="difficulty-level-name">
        ${name}
        </div>
        <div class="difficulty-level-tiers">
        ${tiers}
        </div>
    </button>
    `;
}

function generateDifficultyButton(difficulty, name, level, description, completed) {
    const tierCount = {'muito_facil': 1, 'facil': 2, 'normal': 3, 'dificil': 4, 'muito_dificil': 5}[difficulty];
    let tiers = '';
    for (let i = 1; i <= 5; i++) {
        tiers += `<span class="tier-indicator ${i <= tierCount ? 'filled' : ''}"></span>`;
    }

    const statusIndicator = completed 
        ? `<span class="difficulty-completion-mark" aria-label="${t('common.completed')}">✓</span>`
        : '';

    const borderClass = completed ? 'difficulty-completed' : '';

    return `
        <button class="difficulty-btn difficulty-${DIFFICULTY_MAP[difficulty]} ${borderClass}" 
                onclick="startDailyChallenge('${difficulty}')" 
                data-difficulty="${difficulty}">
        ${statusIndicator}
        <div class="difficulty-level-name">
            ${name}
        </div>
        <div class="difficulty-level-tiers">
            ${tiers}
        </div>
        ${description ? `<div class="difficulty-level-description">${description}</div>` : ''}
        </button>
    `;
}

async function showStatsDashboard() {
    if (!currentUser) {
    alert(t('stats.loginRequired'));
    return;
    }
    setAppRoute('/stats');
    setHeaderControls('stats');
    const appContent = document.getElementById('app-content');
    appContent.innerHTML = `<div class="game-card stats-dashboard">${renderAppState(
        t('stats.loading')
    )}</div>`;
    const loadingCard = appContent.firstElementChild;
    const statsOwnerId = currentUserId;
    const isCurrentRequest = () => appContent.firstElementChild === loadingCard
        && currentUserId === statsOwnerId && Boolean(currentUser);

    let results;
    try {
        results = await Promise.all([
        sb.from('statistics').select('*').eq('user_id', statsOwnerId).single(),
        sb.from('daily_results')
            .select('difficulty, guess_count, won')
            .eq('user_id', statsOwnerId),
        sb.from('daily_results').select('*').eq('user_id', statsOwnerId)
            .order('created_at', { ascending: false }).limit(10),
        sb.from('achievements').select('achievement_id').eq('user_id', statsOwnerId),
        sb.from('daily_results')
            .select('difficulty, guess_count, hint_history, won')
            .eq('user_id', statsOwnerId)
            .eq('won', true)
        ]);
        if (!isCurrentRequest()) return;
        const failedResult = results.find((result, index) => result.error
            && !(index === 0 && result.error.code === 'PGRST116'));
        if (failedResult) throw failedResult.error;
    } catch (error) {
        console.error('Statistics loading failed:', error);
        if (!isCurrentRequest()) return;
        appContent.innerHTML = `<div class="game-card stats-dashboard">${renderAppState(
            t('stats.loadError'), { type: 'error' }
        )}</div>`;
        return;
    }
    const [statsResult, difficultyHistoryResult, recentGamesResult, achievementsResult, achievementHistoryResult] = results;
    const stats = statsResult.data;
    const difficultyHistory = difficultyHistoryResult.data || [];
    const recentGames = recentGamesResult.data;
    const achievements = achievementsResult.data;
    const achievementHistory = achievementHistoryResult.data || [];

    const gamesPlayed = stats?.games_played || 0;
    const gamesWon = stats?.games_won || 0;
    const winRate = gamesPlayed > 0 ? Math.round((gamesWon / gamesPlayed) * 100) : 0;
    const streakData = { current: stats?.current_streak || 0, best: stats?.best_streak || 0, lastPlayed: stats?.last_played };
    let unlockedAchievements = new Set(achievements ? achievements.map(a => a.achievement_id) : []);
    let supplementalAchievementProgress = {};

    try {
        const synchronization = await syncHistoricalAchievements(
            stats,
            achievementHistory,
            unlockedAchievements
        );
        unlockedAchievements = synchronization.unlockedSet;
    } catch (error) {
        console.error('Historical achievement synchronization failed:', error);
    }
    if (!isCurrentRequest()) return;

    try {
        const accountSynchronization = await syncAccountAchievements();
        accountSynchronization.unlockedIds.forEach(id => unlockedAchievements.add(id));
        supplementalAchievementProgress = accountSynchronization.progress;
    } catch (error) {
        console.error('Extended achievement synchronization failed:', error);
    }
    if (!isCurrentRequest()) return;

    const achievementProgress = buildAchievementProgress(
        stats,
        achievementHistory,
        supplementalAchievementProgress
    );
    const unlockedAchievementCount = ACHIEVEMENT_DEFINITIONS
        .filter(achievement => unlockedAchievements.has(achievement.id)).length;

    const safeCurrentUser = escapeChallengeHtml(currentUser);

    appContent.innerHTML = `
    <div class="game-card stats-dashboard">
        <header class="stats-header">
            <div>
                <h2 class="screen-title">${t('stats.title')}</h2>
                <p class="screen-subtitle stats-subtitle">
                    ${t('stats.subtitle')}
                </p>
            </div>
            <div class="stats-player-badge" aria-label="${t('stats.forPlayer', { name: safeCurrentUser })}">
                <span>${t('common.player')}</span>
                <strong>${safeCurrentUser}</strong>
            </div>
        </header>

        <div class="stats stats-overview" aria-label="${t('stats.playerOverview')}">
            <div class="stat"><div class="stat-value">${gamesPlayed}</div><div class="stat-label">${t('stats.gamesPlayed')}</div></div>
            <div class="stat"><div class="stat-value">${gamesWon}</div><div class="stat-label">${t('stats.gamesWon')}</div></div>
            <div class="stat"><div class="stat-value">${winRate}%</div><div class="stat-label">${t('stats.successRate')}</div></div>
            <div class="stat"><div class="stat-value">${stats?.best_score || '-'}</div><div class="stat-label">${t('stats.bestScore')}</div></div>
        </div>

        <div class="stats-insights-grid">
            ${generateStreakDisplay(streakData)}
            <section class="stats-section stats-histogram-section" aria-labelledby="guess-distribution-title">
                <div class="stats-section-heading">
                    <div>
                        <p class="stats-section-kicker">${t('stats.winningPattern')}</p>
                        <h3 class="stats-section-title" id="guess-distribution-title">${t('stats.guessesPerWin')}</h3>
                    </div>
                    <span>${t(gamesWon === 1 ? 'stats.winOne' : 'stats.winMany', { count: gamesWon })}</span>
                </div>
                ${generateGuessHistogram(achievementHistory)}
            </section>
        </div>

        <div class="stats-section">
            <div class="stats-section-heading">
                <div>
                    <p class="stats-section-kicker">${t('stats.difficultyProfile')}</p>
                    <h3 class="stats-section-title">${t('stats.performanceLevel')}</h3>
                </div>
            </div>
            ${generateDifficultyStats(difficultyHistory)}
        </div>

        <div class="achievements-panel">
        <div class="achievements-heading">
            <h3>${t('stats.achievements')}</h3>
            <span>${t('stats.unlockedCount', { count: unlockedAchievementCount, total: ACHIEVEMENT_DEFINITIONS.length })}</span>
        </div>
        ${generateAchievements(unlockedAchievements, achievementProgress)}
        </div>

        <div class="stats-section">
            <div class="stats-section-heading">
                <div>
                    <p class="stats-section-kicker">${t('stats.latestActivity')}</p>
                    <h3 class="stats-section-title">${t('stats.recentGames')}</h3>
                </div>
            </div>
            ${generateRecentGames(recentGames)}
        </div>
    </div>
    `;
    focusAppScreenHeading();
}

function generateStreakDisplay(streakData) {
    if (!currentUser) return '';

    if (!streakData || streakData.current === 0) {
    return `
        <div class="streak-card streak-card-empty">
        <div class="streak-empty-title">${t('stats.noStreak')}</div>
        <div class="streak-copy">${t('stats.noStreakCopy')}</div>
        </div>
    `;
    }

    return `
    <div class="streak-card streak-card-active">
        <div class="streak-value">◆ ${streakData.current}</div>
        <div class="streak-heading">${t('stats.currentStreak')}</div>
        <div class="streak-copy">${t(streakData.best === 1 ? 'stats.bestDayOne' : 'stats.bestDayMany', { count: streakData.best })}</div>
        <div class="streak-meta">${t('stats.lastPlayed', { date: streakData.lastPlayed || t('common.never') })}</div>
    </div>
    `;
}

let mathRendererPromise = null;

function loadPhylosaurScript(src) {
    return new Promise((resolve, reject) => {
        const existing = document.querySelector(`script[src="${src}"]`);
        if (existing) {
            if (existing.dataset.loaded === 'true') resolve();
            else {
                existing.addEventListener('load', resolve, { once: true });
                existing.addEventListener('error', reject, { once: true });
            }
            return;
        }

        const script = document.createElement('script');
        script.src = src;
        script.addEventListener('load', () => {
            script.dataset.loaded = 'true';
            resolve();
        }, { once: true });
        script.addEventListener('error', reject, { once: true });
        document.head.appendChild(script);
    });
}

function ensureMathRenderer() {
    if (window.renderMathInElement) return Promise.resolve();
    if (mathRendererPromise) return mathRendererPromise;

    if (!document.querySelector('link[data-phylosaur-katex]')) {
        const stylesheet = document.createElement('link');
        stylesheet.rel = 'stylesheet';
        stylesheet.href = 'https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.css';
        stylesheet.dataset.phylosaurKatex = 'true';
        document.head.appendChild(stylesheet);
    }

    mathRendererPromise = loadPhylosaurScript('https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.js')
        .then(() => loadPhylosaurScript('https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/contrib/auto-render.min.js'))
        .catch(error => {
            mathRendererPromise = null;
            throw error;
        });
    return mathRendererPromise;
}

async function showAbout() {
    setAppRoute('/about');
    setHeaderControls('about');
    const appContent = document.getElementById('app-content');
    appContent.innerHTML = `<div class="game-card about-screen">${renderAppState(t('about.loading'))}</div>`;

    try {
        const response = await fetch('about.html');
        if (!response.ok) throw new Error(`About page returned ${response.status}.`);
        const aboutDocument = new DOMParser().parseFromString(await response.text(), 'text/html');
        const article = aboutDocument.querySelector('#about-content article');
        if (!article) throw new Error('About content is unavailable.');
        article.querySelector('h2')?.remove();
        article.querySelectorAll('script').forEach(script => script.remove());
        if (getCurrentAppRoute() !== '/about') return;

        appContent.innerHTML = `
            <div class="game-card about-screen">
                <h2 class="screen-title about-screen-title">${t('about.title')}</h2>
                <div class="about-screen-body">${article.innerHTML}</div>
                <button class="btn-new-game" onclick="navigateToAppRoute('/')">${t('about.return')}</button>
            </div>`;
        focusAppScreenHeading();
        const aboutCard = appContent.querySelector('.about-screen');
        await ensureMathRenderer();
        if (!aboutCard?.isConnected || !window.renderMathInElement) return;
        renderMathInElement(aboutCard, {
            delimiters: [
                { left: '$$', right: '$$', display: true },
                { left: '$', right: '$', display: false }
            ],
            throwOnError: false
        });
    } catch (error) {
        if (getCurrentAppRoute() !== '/about') return;
        appContent.innerHTML = `<div class="game-card about-screen">${renderAppState(
            t('about.error'), { type: 'error', detail: error.message }
        )}<button class="btn-new-game" onclick="navigateToAppRoute('/')">${t('about.return')}</button></div>`;
    }
}

async function showHowToPlay() {
    const action = await showModal({
    title: t('home.howToPlay'),
    message: `
        <div class="how-to-play-content">
        <p class="how-to-play-intro">${t('howto.intro')}</p>
        
        <div class="how-to-play-step">
            <strong>${t('howto.step1')}</strong><br>
            ${t('howto.step1Copy')}
        </div>

        <div class="how-to-play-step">
            <strong>${t('howto.step2')}</strong><br>
            ${t('howto.step2Copy')}
        </div>

        <div class="how-to-play-step">
            <strong>${t('howto.step3')}</strong><br>
            ${t('howto.step3Copy')}
        </div>

        <div class="how-to-play-step">
            <strong>${t('howto.step4')}</strong><br>
            ${t('howto.step4Copy')}
        </div>

        <div class="how-to-play-step how-to-play-tip">
            <strong>${t('howto.tip')}</strong><br>
            ${t('howto.tipCopy')}
            <br><br>
            <span class="how-to-play-note">
            ${t('howto.noLimit')}
            </span>
        </div>
        </div>
    `,
    buttons: [
        { text: t('tutorial.interactive'), value: 'tutorial', primary: true },
        { text: t('common.close'), value: 'close', primary: false }
    ],
    closeOnOverlay: true
    });

    if (action === 'tutorial') showInteractiveTutorial();
}

const FIRST_RUN_TUTORIAL_KEY = PHYLOSAUR_STORAGE_KEYS.tutorialComplete;
let tutorialStepIndex = 0;
let tutorialDemoTried = false;
let tutorialKeyHandler = null;
let tutorialPreviouslyFocused = null;
let tutorialPreviousBodyOverflow = '';

const INTERACTIVE_TUTORIAL_STEPS = [
    {
        kicker: t('tutorial.works'),
        title: t('tutorial.hiddenTitle'),
        copy: t('tutorial.hiddenCopy'),
        visual: `
            <svg class="tutorial-welcome-tree" viewBox="0 0 280 150" aria-hidden="true">
                <path class="tutorial-welcome-branch" d="M140 18V52M140 52H62V88M140 52H218V88M62 88H30V122M62 88H94V122M218 88H186V122M218 88H250V122"></path>
                <circle class="tutorial-welcome-node" cx="30" cy="122" r="8"></circle>
                <circle class="tutorial-welcome-node" cx="94" cy="122" r="8"></circle>
                <circle class="tutorial-welcome-node" cx="186" cy="122" r="8"></circle>
                <circle class="tutorial-welcome-node is-target" cx="250" cy="122" r="15"></circle>
                <text class="tutorial-welcome-question" x="250" y="123">?</text>
                <circle class="tutorial-welcome-root" cx="140" cy="18" r="7"></circle>
            </svg>
            <div class="tutorial-welcome-line">
                <span>${t('tutorial.guess')}</span><i class="ui-icon ui-icon-arrow-right" aria-hidden="true"></i>
                <span>${t('tutorial.compare')}</span><i class="ui-icon ui-icon-arrow-right" aria-hidden="true"></i>
                <span>${t('tutorial.followBranches')}</span>
            </div>
        `
    },
    {
        kicker: t('tutorial.step', { count: 1 }),
        title: t('tutorial.makeGuess'),
        copy: t('tutorial.makeGuessCopy'),
        visual: `
            <div class="tutorial-guess-demo">
                <div class="tutorial-fake-input"><em>Triceratops</em></div>
                <button class="tutorial-demo-action" type="button">${t('tutorial.trySample')}</button>
                <div class="tutorial-demo-feedback" aria-live="polite">
                    <strong>Ornithischia</strong>
                    <span>${t('tutorial.sampleResult')}</span>
                </div>
            </div>
        `
    },
    {
        kicker: t('tutorial.step', { count: 2 }),
        title: t('tutorial.followTrail'),
        copy: t('tutorial.followTrailCopy'),
        visual: `
            <div class="tutorial-tree-demo" aria-label="${t('tutorial.exampleTrail')}">
                <div class="tutorial-tree-node is-root">Dinosauria</div>
                <div class="tutorial-tree-link is-best"><i class="ui-icon ui-icon-arrow-down" aria-hidden="true"></i></div>
                <div class="tutorial-tree-node is-best">Ornithischia</div>
                <div class="tutorial-tree-split">
                    <div><span><i class="ui-icon ui-icon-arrow-down-left" aria-hidden="true"></i></span><div class="tutorial-tree-node is-guess">Triceratops</div></div>
                    <div><span class="is-best"><i class="ui-icon ui-icon-arrow-down-right" aria-hidden="true"></i></span><div class="tutorial-tree-node is-best">${t('tutorial.bestTrail')}</div></div>
                </div>
            </div>
        `
    },
    {
        kicker: t('tutorial.step', { count: 3 }),
        title: t('tutorial.usingHints'),
        copy: t('tutorial.usingHintsCopy'),
        visual: `
            <div class="tutorial-hint-demo">
                <div class="tutorial-hint-count"><strong>3</strong><span>${t('tutorial.hintsAvailable')}</span></div>
                <div class="tutorial-hint-rule"><span>◇</span><span>${t('tutorial.guess')}</span><span>◇</span><span>${t('tutorial.guess')}</span><span>◆</span><span>${t('game.hint')}</span></div>
            </div>
        `
    },
    {
        kicker: t('tutorial.chooseLevel'),
        title: t('tutorial.beginLevel'),
        copy: t('tutorial.beginLevelCopy'),
        visual: `
            <div class="tutorial-levels" aria-hidden="true">
                <span class="is-recommended">I<small>${t('tutorial.startLabel')}</small></span>
                <span>II</span><span>III</span><span>IV</span><span>V</span>
            </div>
        `
    }
];

function hasCompletedFirstRunTutorial() {
    try {
        return localStorage.getItem(FIRST_RUN_TUTORIAL_KEY) === 'true';
    } catch (error) {
        return true;
    }
}

function markFirstRunTutorialComplete() {
    try {
        localStorage.setItem(FIRST_RUN_TUTORIAL_KEY, 'true');
    } catch (error) {
        console.warn('Could not save tutorial preference:', error);
    }
}

function maybeShowFirstRunTutorial() {
    if (hasCompletedFirstRunTutorial()) return;
    if (document.querySelector('[data-app-modal="true"], #tutorial-overlay')) return;
    setTimeout(() => {
        if (!hasCompletedFirstRunTutorial() && !document.querySelector('[data-app-modal="true"], #tutorial-overlay')) {
            showInteractiveTutorial({ firstRun: true });
        }
    }, 350);
}

function renderInteractiveTutorialStep() {
    const overlay = document.getElementById('tutorial-overlay');
    if (!overlay) return;
    const step = INTERACTIVE_TUTORIAL_STEPS[tutorialStepIndex];
    const isFirst = tutorialStepIndex === 0;
    const isLast = tutorialStepIndex === INTERACTIVE_TUTORIAL_STEPS.length - 1;
    const requiresDemo = tutorialStepIndex === 1 && !tutorialDemoTried;

    overlay.querySelector('.tutorial-progress').innerHTML = INTERACTIVE_TUTORIAL_STEPS.map((_, index) => `
        <span class="${index === tutorialStepIndex ? 'active' : ''}" aria-label="${t('tutorial.progress', { current: index + 1, total: INTERACTIVE_TUTORIAL_STEPS.length })}"></span>
    `).join('');
    overlay.querySelector('.tutorial-kicker').textContent = step.kicker;
    overlay.querySelector('.tutorial-title').textContent = step.title;
    overlay.querySelector('.tutorial-copy').textContent = step.copy;
    overlay.querySelector('.tutorial-visual').innerHTML = step.visual;

    const backButton = overlay.querySelector('.tutorial-back');
    const nextButton = overlay.querySelector('.tutorial-next');
    backButton.hidden = isFirst;
    nextButton.textContent = isLast
        ? t('tutorial.start')
        : requiresDemo ? t('tutorial.tryFirst') : t('tutorial.next');
    nextButton.disabled = requiresDemo;

    const demoButton = overlay.querySelector('.tutorial-demo-action');
    const demoFeedback = overlay.querySelector('.tutorial-demo-feedback');
    if (tutorialStepIndex === 1 && tutorialDemoTried) {
        demoButton.textContent = t('tutorial.guessRevealed');
        demoButton.disabled = true;
        demoFeedback.classList.add('visible');
    }

    demoButton?.addEventListener('click', event => {
        tutorialDemoTried = true;
        event.currentTarget.textContent = t('tutorial.guessRevealed');
        event.currentTarget.disabled = true;
        demoFeedback?.classList.add('visible');
        nextButton.disabled = false;
        nextButton.textContent = t('tutorial.next');
        nextButton.focus();
    });
}

function closeInteractiveTutorial() {
    const overlay = document.getElementById('tutorial-overlay');
    if (!overlay) return;
    markFirstRunTutorialComplete();
    if (tutorialKeyHandler) document.removeEventListener('keydown', tutorialKeyHandler, true);
    tutorialKeyHandler = null;
    overlay.remove();
    document.body.style.overflow = tutorialPreviousBodyOverflow;
    if (tutorialPreviouslyFocused instanceof HTMLElement && tutorialPreviouslyFocused.isConnected) {
        tutorialPreviouslyFocused.focus();
    }
}

function showInteractiveTutorial({ firstRun = false } = {}) {
    if (document.getElementById('tutorial-overlay')) return;
    tutorialStepIndex = 0;
    tutorialDemoTried = false;
    tutorialPreviouslyFocused = document.activeElement;
    tutorialPreviousBodyOverflow = document.body.style.overflow;

    const overlay = document.createElement('div');
    overlay.id = 'tutorial-overlay';
    overlay.className = 'tutorial-overlay';
    overlay.setAttribute('role', 'dialog');
    overlay.setAttribute('aria-modal', 'true');
    overlay.setAttribute('aria-labelledby', 'tutorial-title');
    overlay.innerHTML = `
        <div class="tutorial-dialog" tabindex="-1">
            <button class="tutorial-skip" type="button">${firstRun ? t('tutorial.skip') : t('common.close')}</button>
            <div class="tutorial-progress" aria-label="${t('tutorial.progressLabel')}"></div>
            <div class="tutorial-kicker"></div>
            <h2 class="tutorial-title" id="tutorial-title"></h2>
            <p class="tutorial-copy"></p>
            <div class="tutorial-visual"></div>
            <div class="tutorial-actions">
                <button class="tutorial-back" type="button">${t('nav.back')}</button>
                <button class="tutorial-next" type="button">${t('tutorial.next')}</button>
            </div>
        </div>
    `;

    document.body.appendChild(overlay);
    document.body.style.overflow = 'hidden';

    overlay.querySelector('.tutorial-skip').addEventListener('click', closeInteractiveTutorial);
    overlay.querySelector('.tutorial-back').addEventListener('click', () => {
        tutorialStepIndex = Math.max(0, tutorialStepIndex - 1);
        renderInteractiveTutorialStep();
    });
    overlay.querySelector('.tutorial-next').addEventListener('click', () => {
        if (tutorialStepIndex === INTERACTIVE_TUTORIAL_STEPS.length - 1) {
            closeInteractiveTutorial();
            return;
        }
        tutorialStepIndex += 1;
        renderInteractiveTutorialStep();
    });

    tutorialKeyHandler = event => {
        if (event.key === 'Escape') {
            event.preventDefault();
            closeInteractiveTutorial();
            return;
        }
        if (event.key !== 'Tab') return;
        const focusable = Array.from(overlay.querySelectorAll('button:not([disabled]):not([hidden])'));
        if (!focusable.length) return;
        const first = focusable[0];
        const last = focusable[focusable.length - 1];
        if (event.shiftKey && document.activeElement === first) {
            event.preventDefault();
            last.focus();
        } else if (!event.shiftKey && document.activeElement === last) {
            event.preventDefault();
            first.focus();
        }
    };
    document.addEventListener('keydown', tutorialKeyHandler, true);
    renderInteractiveTutorialStep();
    overlay.querySelector('.tutorial-dialog').focus();
}

function generateGuessHistogram(wonResults) {
    const buckets = Array.from({ length: 9 }, (_, index) => ({
        label: index < 8 ? String(index + 1) : '9+',
        count: 0
    }));

    (wonResults || []).forEach(result => {
        const guesses = Number(result.guess_count || 0);
        if (!Number.isFinite(guesses) || guesses < 1) return;
        buckets[Math.min(guesses, 9) - 1].count += 1;
    });

    const total = buckets.reduce((sum, bucket) => sum + bucket.count, 0);
    if (total === 0) {
        return `<p class="empty-stats">${t('stats.histogramEmpty')}</p>`;
    }

    const largest = Math.max(...buckets.map(bucket => bucket.count), 1);
    return `
        <div class="stats-histogram" role="img"
             aria-label="${t('stats.distributionLabel', { count: total })}">
            ${buckets.map(bucket => {
                const width = bucket.count === 0
                    ? 0
                    : Math.max(8, Math.round((bucket.count / largest) * 100));
                const guessLabel = t(bucket.label === '1' ? 'game.guessOne' : 'game.guessMany');
                const winLabel = t(bucket.count === 1 ? 'stats.winLabelOne' : 'stats.winLabelMany');
                return `
                    <div class="stats-histogram-row"
                         aria-label="${bucket.label} ${guessLabel}: ${bucket.count} ${winLabel}">
                        <span class="stats-histogram-label">${bucket.label}</span>
                        <span class="stats-histogram-track" aria-hidden="true">
                            <span class="stats-histogram-fill" style="--histogram-width:${width}%"></span>
                        </span>
                        <strong>${bucket.count}</strong>
                    </div>
                `;
            }).join('')}
        </div>
    `;
}

function generateDifficultyStats(difficultyHistory) {
    const diffNames = {
        'muito_facil': t('level.name1'),
        'facil': t('level.name2'),
        'normal': t('level.name3'),
        'dificil': t('level.name4'),
        'muito_dificil': t('level.name5')
    };
    const difficultyOrder = [
        'muito_facil', 'facil', 'normal', 'dificil', 'muito_dificil'
    ];
    const statsByDifficulty = new Map(difficultyOrder.map(difficulty => [difficulty, {
        played: 0,
        won: 0,
        totalGuesses: 0
    }]));

    for (const result of difficultyHistory || []) {
        const stat = statsByDifficulty.get(result.difficulty);
        if (!stat) continue;
        stat.played += 1;
        stat.won += result.won === true ? 1 : 0;
        stat.totalGuesses += Math.max(0, Number(result.guess_count || 0));
    }

    return `<div class="difficulty-stats-list">${difficultyOrder.map(difficulty => {
        const stat = statsByDifficulty.get(difficulty);
        const played = stat.played;
        const won = stat.won;
        const winRate = played > 0 ? Math.round((won / played) * 100) : 0;
        const average = played > 0 ? Math.round(stat.totalGuesses / played) : 0;
        return `
            <article class="difficulty-stat-row">
                <div class="difficulty-stat-heading">
                    <span class="diff-name">${escapeChallengeHtml(diffNames[difficulty])}</span>
                    <span class="diff-record">${t('stats.recordWon', { won, played })}</span>
                </div>
                <div class="difficulty-stat-meter" aria-label="${t('stats.successRateValue', { rate: winRate })}">
                    <span style="--difficulty-stat-width:${winRate}%"></span>
                </div>
                <div class="diff-stats">
                    <span class="diff-winrate">${played
                        ? t('stats.successValue', { rate: winRate })
                        : t('stats.notPlayed')}</span>
                    <span class="diff-avg">${played
                        ? t('stats.averageGuesses', {
                            count: average,
                            unit: t(average === 1 ? 'game.guessOne' : 'game.guessMany')
                        })
                        : t('stats.noAverage')}</span>
                </div>
            </article>
        `;
    }).join('')}</div>`;
}

function generateAchievements(unlockedSet, progressById = {}) {
    const allAchievements = [...ACHIEVEMENT_DEFINITIONS].sort((a, b) =>
        Number(unlockedSet?.has(b.id)) - Number(unlockedSet?.has(a.id))
    );

    let html = '<div class="achievements-grid">';
    allAchievements.forEach(ach => {
    const unlocked = unlockedSet && unlockedSet.has(ach.id);
    const progress = progressById[ach.id] || {
        current: unlocked ? 1 : 0,
        target: 1,
        unit: '',
        complete: unlocked
    };
    const percent = unlocked
        ? 100
        : Math.max(0, Math.min(100, Math.round((progress.current / progress.target) * 100)));
    const legacyUnitKeys = {
        win: 'achievement.unit.win', wins: 'achievement.unit.wins', games: 'achievement.unit.games',
        levels: 'achievement.unit.levels', days: 'achievement.unit.days',
        genera: 'achievement.unit.genera', branches: 'achievement.unit.branches'
    };
    const unitKey = legacyUnitKeys[progress.unit] || progress.unit;
    const progressText = unlocked
        ? t('common.completed')
        : unitKey
            ? t('achievement.progress', {
                current: progress.current,
                target: progress.target,
                unit: t(unitKey)
            })
            : t('achievement.notCompleted');
    html += `
        <div class="achievement-card ${ach.category === 'clade' ? 'achievement-card-clade' : ''} ${unlocked ? 'achievement-unlocked' : 'achievement-locked'}">
        <div class="achievement-card-heading">
            <span class="achievement-medal" aria-hidden="true"></span>
            <div>
                ${ach.category === 'clade' ? `<div class="achievement-category">${t('achievement.cladeCollection')}</div>` : ''}
                <div class="achievement-title">${escapeChallengeHtml(getAchievementName(ach))}</div>
            </div>
        </div>
        <div class="achievement-desc">${escapeChallengeHtml(getAchievementDescription(ach))}</div>
        <div class="achievement-progress" aria-label="${progressText}">
            <div class="achievement-progress-track">
                <div class="achievement-progress-fill" style="width:${percent}%;"></div>
            </div>
            <span>${progressText}</span>
        </div>
        </div>
    `;
    });
    html += '</div>';
    return html;
}

function generateRecentGames(recentGames) {
    if (!recentGames || recentGames.length === 0) {
    return `<p class="empty-stats">${t('stats.noRecent')}</p>`;
    }

    const diffNames = {
    'muito_facil': t('level.name1'),
    'facil': t('level.name2'),
    'normal': t('level.name3'),
    'dificil': t('level.name4'),
    'muito_dificil': t('level.name5')
    };

    const today = getTodayString();

    let html = '<div class="recent-games-list">';
    recentGames.forEach(game => {
    const date = new Date(game.created_at).toLocaleDateString(
        currentLocale === 'pt-BR' ? 'pt-BR' : currentLocale
    );
    const guesses = Number(game.guess_count || 0);
    const isToday = game.played_date === today;
    const spoiler = isToday && !game.won;
    const dinoDisplay = spoiler 
        ? `<span class="recent-game-name recent-game-name-hidden">${t('stats.todayHidden')}</span>`
        : `<span class="recent-game-name"><i>${escapeChallengeHtml(game.target_dino)}</i></span>`;

    html += `
        <article class="recent-game-card ${game.won ? 'recent-game-won' : 'recent-game-incomplete'}">
            <div class="recent-game-primary">
                <span class="recent-game-status" aria-label="${t(game.won ? 'common.completed' : 'achievement.notCompleted')}">${game.won ? '✓' : '—'}</span>
                <div class="recent-game-identification">
                    ${dinoDisplay}
                    <div class="recent-game-meta">
                        <span>${escapeChallengeHtml(diffNames[game.difficulty] || game.difficulty)}</span>
                        <span aria-hidden="true">·</span>
                        <time datetime="${escapeChallengeHtml(game.created_at)}">${escapeChallengeHtml(date)}</time>
                    </div>
                </div>
            </div>
            <div class="recent-game-result">
                <strong>${guesses}</strong>
                <span>${t(guesses === 1 ? 'game.guessOne' : 'game.guessMany')}</span>
            </div>
        </article>
    `;
    });
    return `${html}</div>`;
}

// ═══════════════════════════════════════════════════════════════════════
// MUSEUM SCREEN CONTROLLER
// ═══════════════════════════════════════════════════════════════════════
let selectedMuseumLevel = 'all';
let museumSearchQuery = '';
let museumView = 'atlas';
let selectedMuseumClade = 'all';

const MUSEUM_ATLAS_COLLECTIONS = [
    {
        clade: 'Theropoda',
        title: 'Theropoda',
        descriptionKey: 'museum.atlasTheropoda',
        achievementId: 'theropod_tracker',
        achievementClade: 'Theropoda',
        subclades: ['Ceratosauria', 'Tyrannosauroidea', 'Maniraptora']
    },
    {
        clade: 'Sauropodomorpha',
        title: 'Sauropodomorpha',
        descriptionKey: 'museum.atlasSauropodomorpha',
        achievementId: 'sauropod_collector',
        achievementClade: 'Sauropoda',
        subclades: ['Massopoda', 'Sauropoda', 'Macronaria']
    },
    {
        clade: 'Ornithischia',
        title: 'Ornithischia',
        descriptionKey: 'museum.atlasOrnithischia',
        achievementId: 'ornithischian_explorer',
        achievementClade: 'Ornithischia',
        subclades: ['Thyreophora', 'Ornithopoda', 'Marginocephalia']
    }
];

let museumOverrideCatalogPromise = null;
let museumFallbackCatalogPromise = null;
let museumPaleodataCatalogPromise = null;
let museumLineageCatalogPromise = null;
let museumImageCacheMemory = null;
let museumImageCacheWriteTimer = null;
let museumImageCacheDirty = false;
const MUSEUM_IMAGE_CACHE_WRITE_DELAY_MS = 250;

function getMuseumImageCache() {
    if (museumImageCacheMemory) return museumImageCacheMemory;

    try {
        const stored = JSON.parse(
            localStorage.getItem(PHYLOSAUR_STORAGE_KEYS.museumImageCache) || '{}'
        );
        museumImageCacheMemory = stored && typeof stored === 'object' && !Array.isArray(stored)
            ? stored
            : {};
    } catch (error) {
        console.warn('Museum image cache could not be read:', error);
        museumImageCacheMemory = {};
    }

    return museumImageCacheMemory;
}

function flushMuseumImageCache() {
    if (museumImageCacheWriteTimer) {
        clearTimeout(museumImageCacheWriteTimer);
        museumImageCacheWriteTimer = null;
    }
    if (!museumImageCacheDirty || !museumImageCacheMemory) return;

    try {
        localStorage.setItem(
            PHYLOSAUR_STORAGE_KEYS.museumImageCache,
            JSON.stringify(museumImageCacheMemory)
        );
        museumImageCacheDirty = false;
    } catch (error) {
        console.warn('Museum image cache could not be saved:', error);
    }
}

function scheduleMuseumImageCacheWrite() {
    museumImageCacheDirty = true;
    clearTimeout(museumImageCacheWriteTimer);
    museumImageCacheWriteTimer = setTimeout(
        flushMuseumImageCache,
        MUSEUM_IMAGE_CACHE_WRITE_DELAY_MS
    );
}

window.addEventListener('pagehide', flushMuseumImageCache);
document.addEventListener('visibilitychange', () => {
    if (document.hidden) flushMuseumImageCache();
});

async function ensureMuseumCatalogLineages(catalog) {
    const dinosaurs = Array.isArray(catalog) ? catalog : [];
    if (dinosaurs.every(dino => Array.isArray(dino?.linhagem) && dino.linhagem.length > 0)) {
        return dinosaurs;
    }

    if (!museumLineageCatalogPromise) {
        museumLineageCatalogPromise = fetch('phylosaur_db.json?v=atlas-2')
            .then(response => {
                if (!response.ok) throw new Error(`Lineage catalog HTTP ${response.status}`);
                return response.json();
            })
            .then(records => new Map(
                (Array.isArray(records) ? records : []).map(dino => [
                    String(dino.nome || '').toLowerCase(),
                    Array.isArray(dino.linhagem) ? dino.linhagem : []
                ])
            ))
            .catch(error => {
                console.warn('Museum lineage fallback unavailable:', error);
                return new Map();
            });
    }

    const lineageByName = await museumLineageCatalogPromise;
    return dinosaurs.map(dino => {
        if (Array.isArray(dino?.linhagem) && dino.linhagem.length > 0) return dino;
        const lineage = lineageByName.get(String(dino?.nome || '').toLowerCase()) || [];
        return {
            ...dino,
            linhagem: lineage,
            terminalClade: dino?.terminalClade || lineage.at(-1) || 'Dinosauria'
        };
    });
}

async function loadMuseumPaleodataCatalog() {
    if (!museumPaleodataCatalogPromise) {
        museumPaleodataCatalogPromise = fetch('phylosaur_paleodata.json?v=3')
            .then(response => {
                if (!response.ok) throw new Error(`Paleodata catalog HTTP ${response.status}`);
                return response.json();
            })
            .then(catalog => ({
                timeline: catalog.timeline || { oldest_ma: 251.9, youngest_ma: 66 },
                taxa: catalog.taxa || {}
            }))
            .catch(error => {
                console.warn('Museum paleodata catalog unavailable:', error);
                return { timeline: { oldest_ma: 251.9, youngest_ma: 66 }, taxa: {} };
            });
    }

    return museumPaleodataCatalogPromise;
}

function formatMuseumAge(value) {
    const age = Number(value);
    if (!Number.isFinite(age)) return '';
    return Number.isInteger(age) ? String(age) : age.toFixed(1);
}

const MUSEUM_PALEODATA_TERM_KEYS = Object.freeze({
    'Late Triassic': 'museum.termLateTriassic',
    'Early Jurassic': 'museum.termEarlyJurassic',
    'Middle Jurassic': 'museum.termMiddleJurassic',
    'Late Jurassic': 'museum.termLateJurassic',
    'Early Cretaceous': 'museum.termEarlyCretaceous',
    'Late Cretaceous': 'museum.termLateCretaceous',
    'Aptian–Albian': 'museum.ageAptianAlbian',
    'Bathonian': 'museum.ageBathonian',
    'Callovian': 'museum.ageCallovian',
    'Carnian': 'museum.ageCarnian',
    'Cenomanian–Turonian': 'museum.ageCenomanianTuronian',
    'Early Maastrichtian': 'museum.ageEarlyMaastrichtian',
    'Early Tithonian': 'museum.ageEarlyTithonian',
    'Kimmeridgian–Tithonian': 'museum.ageKimmeridgianTithonian',
    'Late Campanian': 'museum.ageLateCampanian',
    'Late Campanian–Early Maastrichtian': 'museum.ageLateCampanianEarlyMaastrichtian',
    'Late Kimmeridgian': 'museum.ageLateKimmeridgian',
    'Latest Albian, approximately 101.62 ± 0.18 Ma': 'museum.ageLatestAlbian',
    'Maastrichtian': 'museum.ageMaastrichtian',
    'Middle–Late Campanian': 'museum.ageMiddleLateCampanian',
    'Santonian–Campanian': 'museum.ageSantonianCampanian',
    'Tithonian': 'museum.ageTithonian',
    'Triassic': 'museum.triassic',
    'Jurassic': 'museum.jurassic',
    'Cretaceous': 'museum.cretaceous',
    'Europe': 'museum.placeEurope',
    'North America': 'museum.placeNorthAmerica',
    'South America': 'museum.placeSouthAmerica',
    'Antarctica': 'museum.placeAntarctica',
    'Africa': 'museum.placeAfrica',
    'Asia': 'museum.placeAsia',
    'Germany': 'museum.placeGermany',
    'United States': 'museum.placeUnitedStates',
    'Argentina': 'museum.placeArgentina',
    'Madagascar': 'museum.placeMadagascar',
    'Canada': 'museum.placeCanada',
    'Portugal': 'museum.placePortugal',
    'China': 'museum.placeChina',
    'Mongolia': 'museum.placeMongolia',
    'France': 'museum.placeFrance',
    'United Kingdom': 'museum.placeUnitedKingdom',
    'Chile': 'museum.placeChile',
    'Spain': 'museum.placeSpain',
    'Morocco': 'museum.placeMorocco'
});

function localizeMuseumPaleodataTerm(value) {
    const text = String(value || '');
    const key = MUSEUM_PALEODATA_TERM_KEYS[text];
    return key ? t(key) : text;
}

function getMuseumPaleodataSourceUrl(value) {
    try {
        const url = new URL(String(value || ''));
        return ['http:', 'https:'].includes(url.protocol) ? url.href : '';
    } catch (_error) {
        return '';
    }
}

function renderMuseumPaleodataSources(record) {
    const sources = Array.isArray(record?.verification?.sources)
        ? record.verification.sources
        : [];
    const reviewedSources = sources.filter(source =>
        String(source?.citation || '').trim()
        && getMuseumPaleodataSourceUrl(source?.url)
    );
    if (reviewedSources.length === 0) return '';

    const links = reviewedSources.map(source => {
        const citation = escapeChallengeHtml(source?.citation || t('museum.scientificSource'));
        const url = getMuseumPaleodataSourceUrl(source?.url);
        return url
            ? `<a href="${escapeChallengeHtml(url)}" target="_blank" rel="noopener">${citation}</a>`
            : `<span>${citation}</span>`;
    }).join('<b>·</b>');

    return `
        <div class="museum-entry-paleo-sources">
            <strong>${t('museum.reviewedSources')}</strong>
            <div>${links}</div>
        </div>
    `;
}

function renderMuseumPaleodata(record, timeline = {}) {
    const reviewedSources = record?.verification?.sources;
    const hasReviewedSource = Array.isArray(reviewedSources)
        && reviewedSources.some(source =>
            String(source?.citation || '').trim()
            && getMuseumPaleodataSourceUrl(source?.url)
        );
    if (!record
        || record.verification?.status !== 'source_verified'
        || !String(record.verification?.scope || '').trim()
        || !hasReviewedSource) return '';

    const maxMa = record.max_ma === null || record.max_ma === undefined
        ? Number.NaN
        : Number(record.max_ma);
    const minMa = record.min_ma === null || record.min_ma === undefined
        ? Number.NaN
        : Number(record.min_ma);
    const oldestMa = Number(timeline.oldest_ma) || 251.9;
    const youngestMa = Number(timeline.youngest_ma) || 66;
    const timelineSpan = Math.max(1, oldestMa - youngestMa);
    const hasAgeRange = Number.isFinite(maxMa) && Number.isFinite(minMa);
    const rangeStart = hasAgeRange
        ? Math.max(0, Math.min(100, ((oldestMa - maxMa) / timelineSpan) * 100))
        : 0;
    const rangeWidth = hasAgeRange
        ? Math.max(1.2, Math.min(100 - rangeStart, ((maxMa - minMa) / timelineSpan) * 100))
        : 0;
    const ageLabel = record.age_text
        ? String(record.age_text)
        : hasAgeRange
            ? t('museum.millionYearsAgo', {
                oldest: formatMuseumAge(maxMa),
                youngest: formatMuseumAge(minMa)
            })
            : t('museum.ageNotAsserted');
    const countries = Array.isArray(record.countries) ? record.countries : [];
    const continents = Array.isArray(record.continents) ? record.continents : [];
    const formations = Array.isArray(record.formations) ? record.formations : [];
    const locationLabel = countries.length
        ? countries.map(localizeMuseumPaleodataTerm).join(' · ')
        : continents.length
            ? continents.map(localizeMuseumPaleodataTerm).join(' · ')
            : t('museum.locationsReview');

    return `
        <section class="museum-entry-paleodata" aria-label="${t('museum.timeAndLocations')}">
            <div class="museum-entry-paleo-grid">
                <div class="museum-entry-paleo-card museum-entry-paleo-time">
                    <div class="museum-entry-paleo-label">${t('museum.when')}</div>
                    <strong>${escapeChallengeHtml(record.period
                        ? localizeMuseumPaleodataTerm(record.period)
                        : t('museum.intervalReview'))}</strong>
                    <span>${escapeChallengeHtml(localizeMuseumPaleodataTerm(ageLabel))}</span>
                    ${hasAgeRange ? `
                        <div class="museum-time-scale"
                             role="img"
                             aria-label="${escapeChallengeHtml(record.period
                                 ? localizeMuseumPaleodataTerm(record.period)
                                 : t('museum.ageRange'))}, ${escapeChallengeHtml(localizeMuseumPaleodataTerm(ageLabel))}">
                            <div class="museum-time-periods" aria-hidden="true">
                                <span>${t('museum.triassic')}</span><span>${t('museum.jurassic')}</span><span>${t('museum.cretaceous')}</span>
                            </div>
                            <div class="museum-time-track" aria-hidden="true">
                                <span class="museum-time-segment triassic"></span>
                                <span class="museum-time-segment jurassic"></span>
                                <span class="museum-time-segment cretaceous"></span>
                                <i class="museum-time-range" style="left:${rangeStart.toFixed(2)}%; width:${rangeWidth.toFixed(2)}%;"></i>
                            </div>
                            <div class="museum-time-ages" aria-hidden="true">
                                <span>252 Ma</span><span>201</span><span>143</span><span>66 Ma</span>
                            </div>
                        </div>
                    ` : ''}
                </div>

                <div class="museum-entry-paleo-card museum-entry-paleo-place">
                    <div class="museum-entry-paleo-label">${t('museum.where')}</div>
                    <strong>${escapeChallengeHtml(locationLabel)}</strong>
                    ${continents.length ? `
                        <div class="museum-paleo-chips">
                            ${continents.map(continent => `<span>${escapeChallengeHtml(localizeMuseumPaleodataTerm(continent))}</span>`).join('')}
                        </div>
                    ` : ''}
                    ${formations.length ? `
                        <div class="museum-paleo-formations">
                            <b>${t('museum.rockUnits')}</b>
                            <span>${escapeChallengeHtml(formations.join(' · '))}</span>
                        </div>
                    ` : ''}
                </div>
            </div>
            <p class="museum-entry-paleo-note">
                ${t('museum.modernGeographyNote')}
            </p>
            ${currentLocale === 'en' && record.verification?.scope ? `
                <p class="museum-entry-paleo-scope">
                    <strong>${t('museum.reviewedScope')}</strong> ${escapeChallengeHtml(record.verification.scope)}
                </p>
            ` : ''}
            ${renderMuseumPaleodataSources(record)}
        </section>
    `;
}

async function loadMuseumOverrideCatalog() {
    if (!museumOverrideCatalogPromise) {
        museumOverrideCatalogPromise = fetch('phylosaur_media_overrides.json?v=18')
            .then(response => {
                if (!response.ok) throw new Error(`Media overrides HTTP ${response.status}`);
                return response.json();
            })
            .then(catalog => catalog.taxa || {})
            .catch(error => {
                console.warn('Museum media overrides unavailable:', error);
                return {};
            });
    }

    return museumOverrideCatalogPromise;
}

async function loadMuseumFallbackCatalog() {
    if (!museumFallbackCatalogPromise) {
        museumFallbackCatalogPromise = fetch('phylosaur_media_fallback.json')
            .then(response => {
                if (!response.ok) throw new Error(`Media catalog HTTP ${response.status}`);
                return response.json();
            })
            .then(catalog => catalog.taxa || {})
            .catch(error => {
                console.warn('Museum fallback catalog unavailable:', error);
                return {};
            });
    }

    return museumFallbackCatalogPromise;
}

async function getCachedDinoMedia(name) {
    const cache = getMuseumImageCache();
    const overrideCatalog = await loadMuseumOverrideCatalog();
    const override = overrideCatalog[name];

    // Reviewed choices always win, including over images saved by older versions.
    if (override?.url) {
        const media = {
            ...override,
            source: override.source || 'wikimedia'
        };
        if (cache[name]?.url !== media.url) {
            cache[name] = media;
            scheduleMuseumImageCacheWrite();
        }
        return media;
    }

    const cached = cache[name];

    // Older versions stored TotalDino URLs as plain strings.
    if (typeof cached === 'string') {
        return { url: cached, source: 'totaldino' };
    }
    if (cached?.url) return cached;

    // Preserve the current behavior: the exact TotalDino file always wins.
    const totalDinoUrl = await fetchWikimediaImage(name);
    if (totalDinoUrl) {
        const media = {
            url: totalDinoUrl,
            source: 'totaldino'
        };
        cache[name] = media;
        scheduleMuseumImageCacheWrite();
        return media;
    }

    // Only taxa without the current image reach the licensed media fallback.
    const fallbackCatalog = await loadMuseumFallbackCatalog();
    const fallback = fallbackCatalog[name];
    if (fallback?.url) {
        const media = {
            ...fallback,
            source: fallback.source || 'wikimedia'
        };
        cache[name] = media;
        scheduleMuseumImageCacheWrite();
        return media;
    }

    return null;
}

let activeMuseumEntryMedia = null;
let museumEntryEscapeHandler = null;
let museumDiscoveryRecords = {};
const MUSEUM_MEDIA_LOAD_LIMIT = 3;
let museumMediaObserver = null;
let museumMediaLoadQueue = [];
let museumMediaLoadsInFlight = 0;
let museumMediaGeneration = 0;
let museumFilterCards = [];
let museumFilterFrame = null;
let museumSpecimenRenderState = null;
let museumSpecimenRenderFrame = null;

function stopMuseumCardMediaLoading() {
    museumMediaGeneration += 1;
    museumMediaObserver?.disconnect();
    museumMediaObserver = null;
    museumMediaLoadQueue = [];
}

function releaseMuseumViewResources() {
    stopMuseumCardMediaLoading();
    museumFilterCards = [];
    museumSpecimenRenderState = null;
    if (museumFilterFrame !== null) {
        cancelAnimationFrame(museumFilterFrame);
        museumFilterFrame = null;
    }
    if (museumSpecimenRenderFrame !== null) {
        cancelAnimationFrame(museumSpecimenRenderFrame);
        museumSpecimenRenderFrame = null;
    }
    flushMuseumImageCache();
}

function queueMuseumCardMedia(card, generation) {
    if (!card || card.dataset.museumMediaState) return;
    card.dataset.museumMediaState = 'queued';
    museumMediaLoadQueue.push({ card, generation });
    pumpMuseumCardMediaQueue();
}

function pumpMuseumCardMediaQueue() {
    while (museumMediaLoadsInFlight < MUSEUM_MEDIA_LOAD_LIMIT && museumMediaLoadQueue.length) {
        const task = museumMediaLoadQueue.shift();
        museumMediaLoadsInFlight += 1;
        void loadMuseumCardMedia(task)
            .catch(error => {
                if (task.card?.isConnected) task.card.dataset.museumMediaState = 'error';
                console.warn('Museum card media could not be loaded:', error);
            })
            .finally(() => {
                museumMediaLoadsInFlight -= 1;
                pumpMuseumCardMediaQueue();
            });
    }
}

async function loadMuseumCardMedia({ card, generation }) {
    if (!card?.isConnected || generation !== museumMediaGeneration) return;

    card.dataset.museumMediaState = 'loading';
    const image = card.querySelector('.museum-card-art');
    const name = image?.dataset.museumMediaName || '';
    if (!image || !name) return;

    const media = await getCachedDinoMedia(name);
    if (!card.isConnected || generation !== museumMediaGeneration) return;

    image.src = media?.url || 'dinosaur-footprint-1-svgrepo-com.svg';
    image.classList.add('loaded');
    card.dataset.museumMediaState = 'loaded';

    if (media?.source !== 'wikimedia' && media?.source !== 'dinopedia') return;
    const sourceElement = card.querySelector('.museum-card-source');
    if (!sourceElement) return;

    const sourceName = media.source === 'dinopedia' ? 'Dinopedia' : 'Commons';
    const filePage = escapeChallengeHtml(media.file_page || '');
    const attribution = escapeChallengeHtml(media.artist || sourceName);
    const license = escapeChallengeHtml(media.license || '');
    sourceElement.innerHTML = filePage
        ? `<a href="${filePage}" target="_blank" rel="noopener"
              onclick="event.stopPropagation()">${attribution}${license ? ` · ${license}` : ''}</a>`
        : `${attribution}${license ? ` · ${license}` : ''}`;
}

function initializeMuseumCardMediaLoading() {
    stopMuseumCardMediaLoading();
    const generation = museumMediaGeneration;
    const cards = [...document.querySelectorAll('.museum-card.unlocked')]
        .filter(card => card.querySelector('.museum-card-art[data-museum-media-name]'));

    if (!('IntersectionObserver' in window)) {
        cards.forEach(card => queueMuseumCardMedia(card, generation));
        return;
    }

    museumMediaObserver = new IntersectionObserver(entries => {
        entries.forEach(entry => {
            if (!entry.isIntersecting) return;
            museumMediaObserver?.unobserve(entry.target);
            queueMuseumCardMedia(entry.target, generation);
        });
    }, { rootMargin: '400px 0px', threshold: 0.01 });

    cards.forEach(card => museumMediaObserver.observe(card));
}

function formatMuseumDiscoveryDate(value) {
    if (!value) return '';

    let date;
    if (/^\d{4}-\d{2}-\d{2}$/.test(value)) {
        const [year, month, day] = value.split('-').map(Number);
        date = new Date(year, month - 1, day);
    } else {
        date = new Date(value);
    }

    if (Number.isNaN(date.getTime())) return '';
    return date.toLocaleDateString(currentLocale === 'pt-BR' ? 'pt-BR' : currentLocale, {
        year: 'numeric',
        month: 'short',
        day: 'numeric'
    });
}

function getMuseumDiscoverySummary(record) {
    if (!record) {
        return {
            firstLabel: t('museum.unlockDateUnavailable'),
            countLabel: t('museum.unlockedOnce'),
            lastLabel: ''
        };
    }

    const firstDate = formatMuseumDiscoveryDate(record.firstDiscoveredAt);
    const lastDate = formatMuseumDiscoveryDate(record.lastDiscoveredAt);
    const firstLabel = record.firstDateUnknown
        ? t('museum.unlockedBeforeTracking')
        : firstDate
            ? t('museum.firstUnlocked', { date: firstDate })
            : t('museum.unlockDateUnavailable');

    return {
        firstLabel,
        countLabel: record.count === 1
            ? t('museum.unlockedOnce')
            : t('museum.unlockedTimes', { count: record.count }),
        lastLabel: record.count > 1 && lastDate
            ? t('museum.lastUnlocked', { date: lastDate })
            : ''
    };
}

function getMuseumMediaCredit(name, media) {
    if (!media) {
        return `<span>${t('museum.noIllustration')}</span>`;
    }

    if (media.source === 'wikimedia' || media.source === 'dinopedia') {
        const license = media.license_url
            ? `<a href="${media.license_url}" target="_blank" rel="noopener">${media.license}</a>`
            : media.license;
        const sourceName = media.source === 'dinopedia' ? 'Dinopedia' : 'Wikimedia Commons';
        const contributor = escapeChallengeHtml(
            media.artist || t('media.contributor', { source: sourceName })
        );
        const editorialNote = currentLocale === 'en' && media.editorial_note
            ? `<span class="museum-entry-media-note">${escapeChallengeHtml(media.editorial_note)}</span>`
            : '';
        return `
            ${t('media.imageBy', { contributor })} · ${license}
            · <a href="${media.file_page}" target="_blank" rel="noopener">${sourceName}</a>
            ${editorialNote}
        `;
    }

    const commonsPage = `https://commons.wikimedia.org/wiki/File:${encodeURIComponent(name + ' TD.png')}`;
    return `
        ${t('media.imageSource')} <a href="${commonsPage}" target="_blank" rel="noopener">Wikimedia Commons</a>
    `;
}

async function closeMuseumEntry({ animate = false, restorePosition = false } = {}) {
    const overlay = document.getElementById('museum-entry-overlay');
    if (animate && overlay) {
        if (!overlay.museumClosing) {
            overlay.classList.add('is-closing');
            const duration = window.matchMedia('(prefers-reduced-motion: reduce)').matches ? 0 : 180;
            overlay.museumClosing = new Promise(resolve => setTimeout(resolve, duration));
        }
        await overlay.museumClosing;
        if (document.getElementById('museum-entry-overlay') !== overlay) return false;
    }

    document.getElementById('museum-image-viewer')?.remove();
    overlay?.remove();
    document.body.style.overflow = overlay?.museumReturnState?.overflow || '';
    activeMuseumEntryMedia = null;

    if (museumEntryEscapeHandler) {
        document.removeEventListener('keydown', museumEntryEscapeHandler);
        museumEntryEscapeHandler = null;
    }

    if (restorePosition && overlay?.museumReturnState) {
        const { scrollX, scrollY, card } = overlay.museumReturnState;
        window.scrollTo({ left: scrollX, top: scrollY, behavior: 'instant' });
        if (card?.isConnected) card.focus({ preventScroll: true });
    }
    return true;
}

async function dismissMuseumEntry() {
    const overlay = document.getElementById('museum-entry-overlay');
    if (!overlay || overlay.museumDismissRequested) return;
    overlay.museumDismissRequested = true;
    const route = getCurrentAppRoute();
    const closed = await closeMuseumEntry({ animate: true, restorePosition: true });
    if (closed && getCurrentAppRoute() === route && route.startsWith('/museum/')) {
        if (Number(window.history.state?.phylosaurDepth || 0) > 0) {
            window.history.back();
        } else {
            setAppRoute('/museum', { replace: true });
        }
    }
}

function openMuseumImageViewer() {
    if (!activeMuseumEntryMedia?.url) return;

    document.getElementById('museum-image-viewer')?.remove();
    const viewer = document.createElement('div');
    viewer.id = 'museum-image-viewer';
    viewer.className = 'museum-image-viewer';
    viewer.innerHTML = `
        <button class="museum-image-viewer-close" type="button" aria-label="${t('museum.closeImage')}">×</button>
        <img src="${activeMuseumEntryMedia.url}" alt="${activeMuseumEntryMedia.name}">
        <div class="museum-image-viewer-caption">
            <em>${activeMuseumEntryMedia.name}</em>
            <div>${activeMuseumEntryMedia.credit}</div>
        </div>
    `;

    viewer.addEventListener('click', event => {
        if (event.target === viewer || event.target.closest('.museum-image-viewer-close')) {
            viewer.remove();
        }
    });

    document.body.appendChild(viewer);
}

async function showMuseumEntry(name) {
    setAppRoute(`/museum/${encodeURIComponent(name)}`);
    closeMuseumEntry();

    const dino = fullDatabase.find(item => item.nome === name);
    if (!dino) return;
    const safeName = escapeChallengeHtml(name);

    const overlay = document.createElement('div');
    overlay.id = 'museum-entry-overlay';
    overlay.className = 'museum-entry-overlay';
    overlay.museumReturnState = {
        scrollX: window.scrollX,
        scrollY: window.scrollY,
        overflow: document.body.style.overflow,
        card: Array.from(document.querySelectorAll('.museum-card.unlocked'))
            .find(card => card.dataset.museumName === name.toLowerCase()) || document.activeElement
    };
    overlay.innerHTML = `
        <article class="museum-entry-dialog" role="dialog" aria-modal="true" aria-label="${safeName}">
            <button class="museum-entry-close" type="button" onclick="dismissMuseumEntry()" aria-label="${t('common.close')}">×</button>
            <div class="museum-entry-loading">${t('museum.openingName', { name: safeName })}</div>
        </article>
    `;

    overlay.addEventListener('click', event => {
        if (event.target === overlay) dismissMuseumEntry();
    });

    document.body.appendChild(overlay);
    document.body.style.overflow = 'hidden';

    museumEntryEscapeHandler = event => {
        if (event.key !== 'Escape') return;
        const viewer = document.getElementById('museum-image-viewer');
        if (viewer) viewer.remove();
        else dismissMuseumEntry();
    };
    document.addEventListener('keydown', museumEntryEscapeHandler);

    if (!Array.isArray(dino.linhagem)) {
        try {
            const discoveryRecord = museumDiscoveryRecords[name.toLowerCase()];
            const entry = await callGameApi('museum_entry', {
                name,
                museumProof: discoveryRecord?.museumProof || null,
                proofSessionIds: getStoredGameSessionIds()
            });
            Object.assign(dino, entry.dinosaur);
        } catch (error) {
            overlay.querySelector('.museum-entry-dialog').innerHTML = `
                <button class="museum-entry-close" type="button" onclick="dismissMuseumEntry()" aria-label="${t('common.close')}">×</button>
                <div class="museum-entry-loading" style="color:var(--color-danger);">
                    ${t('museum.openNameError', { name: safeName })}<br>${escapeChallengeHtml(error.message)}
                </div>
            `;
            return;
        }
    }

    if (!document.body.contains(overlay)) return;

    const mediaPromise = getCachedDinoMedia(name);
    const wikiPromise = fetchWikipediaInfo(name);
    const paleodataPromise = loadMuseumPaleodataCatalog();
    const levelNames = {
        muito_facil: t('level.name1'),
        facil: t('level.name2'),
        normal: t('level.name3'),
        dificil: t('level.name4'),
        muito_dificil: t('level.name5')
    };
    const lineage = (dino.linhagem || [])
        .map(clade => `<span>${escapeChallengeHtml(clade)}</span>`)
        .join('<b>›</b>');
    const discovery = getMuseumDiscoverySummary(
        museumDiscoveryRecords[name.toLowerCase()]
    );

    activeMuseumEntryMedia = {
        name,
        url: null,
        credit: ''
    };

    overlay.querySelector('.museum-entry-dialog').innerHTML = `
            <button class="museum-entry-close" type="button" onclick="dismissMuseumEntry()" aria-label="${t('common.close')}">×</button>

        <header class="museum-entry-header">
            <div class="museum-entry-kicker">${t('museum.entry')}</div>
            <h2>${safeName}</h2>
            <div class="museum-entry-meta">
                ${levelNames[dino.dificuldade] || dino.dificuldade}
                · ${(dino.linhagem || []).at(-1) || 'Dinosauria'}
            </div>
            <div class="museum-entry-discovery">
                <span>${discovery.firstLabel}</span>
                <strong>${discovery.countLabel}</strong>
                ${discovery.lastLabel ? `<span>${discovery.lastLabel}</span>` : ''}
            </div>
        </header>

        <div class="museum-entry-layout">
            <figure class="museum-entry-figure">
                <button class="museum-entry-image-button" type="button"
                        onclick="openMuseumImageViewer()"
                        disabled
                        aria-label="${t('museum.viewLarger', { name: safeName })}">
                    <img src="dinosaur-footprint-1-svgrepo-com.svg" alt="${safeName}">
                </button>
                <figcaption>${t('museum.loadingIllustration')}</figcaption>
            </figure>

            <section class="museum-entry-copy">
                <div class="museum-entry-ornament">◆</div>
                <p class="museum-entry-description">
                    ${t('museum.loadingOverview')}
                </p>

                <div class="museum-entry-paleodata-slot">
                    <div class="museum-entry-section-loading">${t('museum.loadingFossils')}</div>
                </div>

                <h3>${t('museum.classification')}</h3>
                <div class="museum-entry-lineage">${lineage || '<span>Dinosauria</span>'}</div>

                <div class="museum-entry-read-more-slot"></div>
            </section>
        </div>
    `;

    void mediaPromise.then(media => {
        if (!overlay.isConnected) return;
        const figure = overlay.querySelector('.museum-entry-figure');
        const button = figure?.querySelector('.museum-entry-image-button');
        const image = figure?.querySelector('img');
        const caption = figure?.querySelector('figcaption');
        if (!button || !image || !caption) return;

        const hasImage = Boolean(media?.url);
        const credit = hasImage
            ? getMuseumMediaCredit(name, media)
            : t('museum.noIllustration');
        image.src = media?.url || 'dinosaur-footprint-1-svgrepo-com.svg';
        button.disabled = !hasImage;
        if (hasImage) button.insertAdjacentHTML('beforeend', `<span>${t('museum.clickEnlarge')}</span>`);
        caption.innerHTML = credit;
        activeMuseumEntryMedia = { name, url: media?.url || null, credit };
    }).catch(error => {
        console.warn(`Museum illustration unavailable for ${name}:`, error);
        const caption = overlay.querySelector('.museum-entry-figure figcaption');
        if (caption) caption.textContent = t('museum.noIllustration');
    });

    void wikiPromise.then(wikiInfo => {
        if (!overlay.isConnected) return;
        const description = overlay.querySelector('.museum-entry-description');
        if (description) {
            description.textContent = wikiInfo?.description
                || t('museum.noSummary');
        }

        const readMoreSlot = overlay.querySelector('.museum-entry-read-more-slot');
        if (readMoreSlot && wikiInfo?.url) {
            readMoreSlot.innerHTML = `
                <a class="museum-entry-read-more" href="${escapeChallengeHtml(wikiInfo.url)}"
                   target="_blank" rel="noopener">
                    <span>${t('museum.readWikipedia')}</span>
                    <i class="ui-icon ui-icon-external" aria-hidden="true"></i>
                </a>
            `;
        }
    });

    void paleodataPromise.then(paleodataCatalog => {
        if (!overlay.isConnected) return;
        const slot = overlay.querySelector('.museum-entry-paleodata-slot');
        if (!slot) return;
        const paleodata = paleodataCatalog.taxa[name] || null;
        const html = renderMuseumPaleodata(paleodata, paleodataCatalog.timeline);
        if (html) slot.innerHTML = html;
        else slot.remove();
    });
}

function getMuseumAtlasCollection(definition, unlockedSet) {
    const specimens = fullDatabase.filter(dino =>
        Array.isArray(dino.linhagem) && dino.linhagem.includes(definition.clade)
    );
    const unlockedSpecimens = specimens.filter(dino => unlockedSet.has(dino.nome.toLowerCase()));
    const achievementDefinition = CLADE_ACHIEVEMENT_DEFINITIONS.find(
        achievement => achievement.id === definition.achievementId
    );
    const achievementTarget = Number(achievementDefinition?.target || 10);
    const achievementCount = fullDatabase.filter(dino =>
        unlockedSet.has(dino.nome.toLowerCase()) &&
        Array.isArray(dino.linhagem) &&
        dino.linhagem.includes(definition.achievementClade)
    ).length;
    const subclades = definition.subclades.map(clade => {
        const members = specimens.filter(dino => dino.linhagem.includes(clade));
        return {
            clade,
            total: members.length,
            unlocked: members.filter(dino => unlockedSet.has(dino.nome.toLowerCase())).length
        };
    });

    return {
        ...definition,
        total: specimens.length,
        unlocked: unlockedSpecimens.length,
        percent: specimens.length ? Math.round((unlockedSpecimens.length / specimens.length) * 100) : 0,
        achievementName: achievementDefinition
            ? getAchievementName(achievementDefinition)
            : t('achievement.collectionMilestone'),
        achievementTarget,
        achievementCount,
        achievementComplete: achievementCount >= achievementTarget,
        subclades
    };
}

function renderMuseumAtlas(unlockedSet) {
    const collections = MUSEUM_ATLAS_COLLECTIONS.map(definition =>
        getMuseumAtlasCollection(definition, unlockedSet)
    );

    return `
        <section class="museum-atlas" aria-labelledby="museum-atlas-title">
            <div class="museum-atlas-intro">
                <div>
                    <h3 id="museum-atlas-title">${t('museum.taxonomicOverview')}</h3>
                    <p>${t('museum.atlasIntro')}</p>
                </div>
            </div>

            <div class="museum-atlas-grid">
                ${collections.map(collection => {
                    const achievementCurrent = Math.min(
                        collection.achievementCount,
                        collection.achievementTarget
                    );
                    const achievementPercent = Math.round(
                        (achievementCurrent / collection.achievementTarget) * 100
                    );
                    const subclades = collection.subclades.map(subclade => `
                        <span title="${t('museum.discoveredTitle', {
                            unlocked: subclade.unlocked,
                            total: subclade.total
                        })}">
                            ${escapeChallengeHtml(subclade.clade)}
                            <small>${subclade.unlocked}/${subclade.total}</small>
                        </span>
                    `).join('');
                    return `
                        <button class="museum-atlas-card museum-atlas-${collection.clade.toLowerCase()}"
                                type="button"
                                onclick="openMuseumClade('${collection.clade}')"
                                aria-label="${t('museum.exploreClade', {
                                    clade: collection.title,
                                    unlocked: collection.unlocked,
                                    total: collection.total
                                })}">
                            <strong class="museum-atlas-card-title">${collection.title}</strong>
                            <span class="museum-atlas-card-description">${t(collection.descriptionKey)}</span>

                            <span class="museum-atlas-count">
                                <strong>${collection.unlocked}</strong>
                                <span>${t('museum.discoveredCount', { total: collection.total })}</span>
                                <small>${t('museum.branchPercent', { percent: collection.percent })}</small>
                            </span>
                            <span class="museum-atlas-progress" aria-hidden="true">
                                <span style="width:${collection.percent}%"></span>
                            </span>

                            <span class="museum-atlas-subclades-label">${t('museum.selectedSubclades')}</span>
                            <span class="museum-atlas-subclades">${subclades}</span>

                            <span class="museum-atlas-achievement ${collection.achievementComplete ? 'is-complete' : ''}">
                                <span>${escapeChallengeHtml(collection.achievementName)}</span>
                                <strong>${achievementCurrent}/${collection.achievementTarget}</strong>
                                <i><span style="width:${achievementPercent}%"></span></i>
                            </span>

                            <span class="museum-atlas-open">${t('museum.filterByClade')}</span>
                        </button>
                    `;
                }).join('')}
            </div>

            <p class="museum-atlas-note">
                ${t('museum.atlasOutsideNote')}
            </p>
        </section>
    `;
}

function renderMuseumSpecimenCard(dino, unlockedSet) {
    const normalizedName = String(dino.nome || '').toLowerCase();
    const safeName = escapeChallengeHtml(dino.nome || t('museum.unknownGenus'));
    const isUnlocked = unlockedSet.has(normalizedName);
    const lastClade = dino.terminalClade || dino.linhagem?.at(-1) || 'Dinosauria';
    const lineageData = Array.isArray(dino.linhagem) ? dino.linhagem.join('|') : '';
    const cardData = `data-museum-level="${escapeChallengeHtml(dino.dificuldade)}" data-museum-name="${escapeChallengeHtml(normalizedName)}" data-museum-unlocked="${isUnlocked}" data-museum-lineage="${escapeChallengeHtml(lineageData)}"`;

    if (!isUnlocked) {
        return `
            <div class="museum-card locked difficulty-${DIFFICULTY_MAP[dino.dificuldade]}" ${cardData}>
                <div class="museum-card-art-container">
                    <span class="museum-card-lock-icon" aria-hidden="true"></span>
                </div>
                <div class="museum-card-name">???</div>
                <div class="museum-card-clade">${t('museum.locked')}</div>
            </div>
        `;
    }

    const discovery = getMuseumDiscoverySummary(museumDiscoveryRecords[normalizedName]);
    return `
        <div class="museum-card unlocked difficulty-${DIFFICULTY_MAP[dino.dificuldade]}" ${cardData}
             data-museum-entry="${safeName}" role="button" tabindex="0"
             aria-label="${t('museum.openEntryFor', { name: safeName })}">
            <div class="museum-card-art-container">
                <img class="museum-card-art"
                     data-museum-media-name="${safeName}"
                     src="dinosaur-footprint-1-svgrepo-com.svg"
                     alt="${safeName}" loading="lazy" decoding="async" />
            </div>
            <div class="museum-card-name">${safeName}</div>
            <div class="museum-card-clade">${escapeChallengeHtml(lastClade)}</div>
            <div class="museum-card-discovery">
                <span>${escapeChallengeHtml(discovery.firstLabel)}</span>
                ${museumDiscoveryRecords[normalizedName]?.count > 1
                    ? `<strong>${escapeChallengeHtml(discovery.countLabel)}</strong>`
                    : ''}
            </div>
            <div class="museum-card-source"></div>
        </div>
    `;
}

function handleMuseumSpecimenActivation(event) {
    if (event.type === 'keydown' && event.key !== 'Enter' && event.key !== ' ') return;
    const card = event.target?.closest?.('.museum-card.unlocked[data-museum-entry]');
    const grid = document.getElementById('museum-grid');
    if (!card || !grid?.contains(card)) return;
    if (event.type === 'keydown') event.preventDefault();
    showMuseumEntry(card.dataset.museumEntry);
}

function renderMuseumSpecimensNow() {
    const grid = document.getElementById('museum-grid');
    const state = museumSpecimenRenderState;
    if (!grid || !state) return false;

    grid.innerHTML = state.dinosaurs
        .map(dino => renderMuseumSpecimenCard(dino, state.unlockedSet))
        .join('') + `
            <div class="museum-empty-state" id="museum-empty-state" hidden>
                ${t('museum.noMatch')}
            </div>
        `;
    grid.dataset.renderState = 'rendered';
    grid.classList.remove('museum-grid-pending');
    grid.addEventListener('click', handleMuseumSpecimenActivation);
    grid.addEventListener('keydown', handleMuseumSpecimenActivation);
    museumFilterCards = [...grid.querySelectorAll('.museum-card[data-museum-level]')];
    applyMuseumFilters();
    initializeMuseumCardMediaLoading();
    return true;
}

function ensureMuseumSpecimensRendered() {
    const grid = document.getElementById('museum-grid');
    if (!grid || !museumSpecimenRenderState) return false;
    if (grid.dataset.renderState === 'rendered') return true;
    if (grid.dataset.renderState === 'scheduled') return false;

    grid.dataset.renderState = 'scheduled';
    grid.classList.add('museum-grid-pending');
    grid.innerHTML = renderAppState(t('museum.preparingSpecimens'), { compact: true });
    museumSpecimenRenderFrame = requestAnimationFrame(() => {
        museumSpecimenRenderFrame = null;
        if (!grid.isConnected) return;
        if (museumView !== 'specimens') {
            grid.dataset.renderState = 'pending';
            grid.classList.remove('museum-grid-pending');
            grid.innerHTML = '';
            return;
        }
        renderMuseumSpecimensNow();
    });
    return false;
}

function switchMuseumView(view) {
    museumView = view === 'specimens' ? 'specimens' : 'atlas';
    document.querySelectorAll('[data-museum-view]').forEach(button => {
        const active = button.dataset.museumView === museumView;
        button.classList.toggle('active', active);
        button.setAttribute('aria-selected', String(active));
    });

    const atlasPanel = document.getElementById('museum-atlas-panel');
    const specimensPanel = document.getElementById('museum-specimens-panel');
    if (atlasPanel) atlasPanel.hidden = museumView !== 'atlas';
    if (specimensPanel) specimensPanel.hidden = museumView !== 'specimens';

    if (museumView === 'specimens') {
        if (ensureMuseumSpecimensRendered()) {
            applyMuseumFilters();
            initializeMuseumCardMediaLoading();
        }
    } else {
        stopMuseumCardMediaLoading();
    }
}

function updateMuseumCladeFilterDisplay() {
    const banner = document.getElementById('museum-clade-filter');
    const name = document.getElementById('museum-clade-filter-name');
    if (!banner || !name) return;
    banner.hidden = selectedMuseumClade === 'all';
    name.textContent = selectedMuseumClade === 'all' ? '' : selectedMuseumClade;
}

function openMuseumClade(clade) {
    selectedMuseumClade = clade;
    selectedMuseumLevel = 'all';
    museumSearchQuery = '';

    const input = document.getElementById('museum-search-input');
    if (input) input.value = '';
    document.querySelectorAll('[data-museum-filter]').forEach(button => {
        const active = button.dataset.museumFilter === 'all';
        button.classList.toggle('active', active);
        button.setAttribute('aria-pressed', String(active));
    });
    updateMuseumCladeFilterDisplay();
    switchMuseumView('specimens');
}

function clearMuseumCladeFilter() {
    selectedMuseumClade = 'all';
    updateMuseumCladeFilterDisplay();
    applyMuseumFilters();
}

async function showMuseum() {
    setAppRoute('/museum');
    setHeaderControls('museum');
    const appContent = document.getElementById('app-content');
    
    appContent.innerHTML = `<div class="game-card">${renderAppState(t('museum.loading'))}</div>`;
    const loadingCard = appContent.firstElementChild;
    const museumOwnerId = currentUserId;
    const isCurrentRequest = () => appContent.firstElementChild === loadingCard
        && currentUserId === museumOwnerId;
    
    try {
        let museumDatabase = fullDatabase;
        if (!fullDatabase || fullDatabase.length === 0) {
            const catalog = await callGameApi('catalog');
            if (!isCurrentRequest()) return;
            museumDatabase = catalog.dinosaurs || [];
        }
        const [catalogDatabase, discoveryRecords] = await Promise.all([
            ensureMuseumCatalogLineages(museumDatabase),
            getDiscoveryRecords()
        ]);
        if (!isCurrentRequest()) return;
        fullDatabase = catalogDatabase;
        museumDiscoveryRecords = discoveryRecords;
        if (!currentUserId) synchronizeGuestAchievements();
        const unlockedList = Object.values(museumDiscoveryRecords)
            .map(record => record.name);
        const unlockedSet = new Set(unlockedList.map(name => name.toLowerCase()));

        const museumDinos = [...fullDatabase]
            .sort((a, b) => a.nome.localeCompare(b.nome));
        museumSpecimenRenderState = { dinosaurs: museumDinos, unlockedSet };
        museumFilterCards = [];
        
        const totalCount = fullDatabase.length;
        const totalUnlocked = fullDatabase
            .filter(dino => unlockedSet.has(dino.nome.toLowerCase())).length;
        const totalPercent = totalCount > 0 ? Math.round((totalUnlocked / totalCount) * 100) : 0;

        const html = `
            <div class="game-card">
                <h2 class="screen-title">${t('museum.title')}</h2>
                
                <div class="museum-progress-container">
                    <div style="font-size:1.1em; color:var(--color-secondary); font-weight:600;">
                        ${t('museum.progress', { unlocked: totalUnlocked, total: totalCount, percent: totalPercent })}
                    </div>
                    <div class="museum-progress-bar">
                        <div class="museum-progress-fill" style="width: ${totalPercent}%;"></div>
                    </div>
                    <div style="font-size:0.85em; color:var(--color-muted); font-style:italic;">
                        ${t('museum.unlockCopy')}
                    </div>
                </div>

                <div class="museum-view-switch" role="tablist" aria-label="${t('museum.view')}">
                    <button type="button" role="tab" data-museum-view="atlas"
                            class="${museumView === 'atlas' ? 'active' : ''}"
                            aria-selected="${museumView === 'atlas'}"
                            aria-controls="museum-atlas-panel"
                            onclick="switchMuseumView('atlas')">${t('museum.atlas')}</button>
                    <button type="button" role="tab" data-museum-view="specimens"
                            class="${museumView === 'specimens' ? 'active' : ''}"
                            aria-selected="${museumView === 'specimens'}"
                            aria-controls="museum-specimens-panel"
                            onclick="switchMuseumView('specimens')">${t('museum.specimens')}</button>
                </div>

                <div id="museum-atlas-panel" role="tabpanel" ${museumView === 'atlas' ? '' : 'hidden'}>
                    ${renderMuseumAtlas(unlockedSet)}
                </div>

                <div id="museum-specimens-panel" role="tabpanel" ${museumView === 'specimens' ? '' : 'hidden'}>
                <div class="museum-clade-filter" id="museum-clade-filter" ${selectedMuseumClade === 'all' ? 'hidden' : ''}>
                    <span>${t('museum.exploring')} <strong id="museum-clade-filter-name">${escapeChallengeHtml(selectedMuseumClade === 'all' ? '' : selectedMuseumClade)}</strong></span>
                    <button type="button" onclick="clearMuseumCladeFilter()">${t('museum.showAllClades')}</button>
                </div>

                <div class="museum-toolbar">
                    <label class="museum-search" for="museum-search-input">
                        <span>${t('museum.search')}</span>
                        <input id="museum-search-input" type="search"
                               value="${escapeChallengeHtml(museumSearchQuery)}"
                               placeholder="${t('museum.searchPlaceholder')}"
                               autocomplete="off"
                               oninput="updateMuseumSearch(this.value)">
                    </label>

                    <div class="tab-row museum-tabs" role="group" aria-label="${t('museum.filterLevel')}">
                        <button class="tab-btn museum-filter-all ${selectedMuseumLevel === 'all' ? 'active' : ''}"
                                data-museum-filter="all" aria-pressed="${selectedMuseumLevel === 'all'}"
                                onclick="switchMuseumLevel('all')">${t('museum.all')}</button>
                        <button class="tab-btn museum-filter-very-easy ${selectedMuseumLevel === 'muito_facil' ? 'active' : ''}"
                                data-museum-filter="muito_facil" aria-pressed="${selectedMuseumLevel === 'muito_facil'}"
                                onclick="switchMuseumLevel('muito_facil')">${t('level.name1')}</button>
                        <button class="tab-btn museum-filter-easy ${selectedMuseumLevel === 'facil' ? 'active' : ''}"
                                data-museum-filter="facil" aria-pressed="${selectedMuseumLevel === 'facil'}"
                                onclick="switchMuseumLevel('facil')">${t('level.name2')}</button>
                        <button class="tab-btn museum-filter-normal ${selectedMuseumLevel === 'normal' ? 'active' : ''}"
                                data-museum-filter="normal" aria-pressed="${selectedMuseumLevel === 'normal'}"
                                onclick="switchMuseumLevel('normal')">${t('level.name3')}</button>
                        <button class="tab-btn museum-filter-hard ${selectedMuseumLevel === 'dificil' ? 'active' : ''}"
                                data-museum-filter="dificil" aria-pressed="${selectedMuseumLevel === 'dificil'}"
                                onclick="switchMuseumLevel('dificil')">${t('level.name4')}</button>
                        <button class="tab-btn museum-filter-very-hard ${selectedMuseumLevel === 'muito_dificil' ? 'active' : ''}"
                                data-museum-filter="muito_dificil" aria-pressed="${selectedMuseumLevel === 'muito_dificil'}"
                                onclick="switchMuseumLevel('muito_dificil')">${t('level.name5')}</button>
                    </div>
                </div>

                <div class="museum-filter-summary" id="museum-filter-summary" aria-live="polite">
                    ${t('museum.showing', {
                        count: totalCount,
                        specimens: t('museum.specimenMany'),
                        clade: '',
                        unlocked: totalUnlocked
                    })}
                </div>

                <div class="museum-grid" id="museum-grid" data-render-state="pending">
                </div>
                </div>
            </div>
            <div id="clade-info"></div>`;
        appContent.innerHTML = html;

        updateMuseumCladeFilterDisplay();
        switchMuseumView(museumView);
        focusAppScreenHeading();

    } catch (err) {
        console.error('Museum Error:', err);
        if (!isCurrentRequest()) return;
        appContent.innerHTML = `<div class="game-card">${renderAppState(t('museum.loadError'), {
            type: 'error', detail: err.message
        })}</div>`;
    }
}

function switchMuseumLevel(level) {
    selectedMuseumLevel = level;
    document.querySelectorAll('[data-museum-filter]').forEach(button => {
        const isActive = button.dataset.museumFilter === level;
        button.classList.toggle('active', isActive);
        button.setAttribute('aria-pressed', String(isActive));
    });
    applyMuseumFilters();
}

function updateMuseumSearch(value) {
    museumSearchQuery = String(value || '').trim().toLowerCase();
    if (museumFilterFrame !== null) return;
    museumFilterFrame = requestAnimationFrame(() => {
        museumFilterFrame = null;
        applyMuseumFilters();
    });
}

function applyMuseumFilters() {
    const cards = museumFilterCards.length
        ? museumFilterCards
        : [...document.querySelectorAll('.museum-card[data-museum-level]')];
    if (cards.length === 0) return;

    let visibleCount = 0;
    let visibleUnlocked = 0;

    cards.forEach(card => {
        const matchesLevel = selectedMuseumLevel === 'all'
            || card.dataset.museumLevel === selectedMuseumLevel;
        const matchesSearch = !museumSearchQuery
            || card.dataset.museumName.includes(museumSearchQuery);
        const matchesClade = selectedMuseumClade === 'all'
            || `|${card.dataset.museumLineage || ''}|`.includes(`|${selectedMuseumClade}|`);
        const isVisible = matchesLevel && matchesSearch && matchesClade;

        card.hidden = !isVisible;
        if (!isVisible) return;

        visibleCount += 1;
        if (card.dataset.museumUnlocked === 'true') visibleUnlocked += 1;
    });

    const summary = document.getElementById('museum-filter-summary');
    if (summary) {
        const specimenLabel = visibleCount === 1
            ? t('museum.specimenOne')
            : t('museum.specimenMany');
        const cladeLabel = selectedMuseumClade === 'all'
            ? ''
            : t('museum.inClade', { clade: selectedMuseumClade });
        summary.textContent = t('museum.showing', {
            count: visibleCount,
            specimens: specimenLabel,
            clade: cladeLabel,
            unlocked: visibleUnlocked
        });
    }

    const emptyState = document.getElementById('museum-empty-state');
    if (emptyState) {
        emptyState.hidden = visibleCount !== 0;
        emptyState.textContent = selectedMuseumClade === 'all'
            ? t('museum.noMatch')
            : t('museum.noCladeMatch', { clade: selectedMuseumClade });
    }
}

function analyticsLabel(value) {
    const labels = {
        daily: 'analytics.daily', practice: 'analytics.practice', challenge: 'analytics.friends',
        muito_facil: 'level.name1', facil: 'level.name2', normal: 'level.name3',
        dificil: 'level.name4', muito_dificil: 'level.name5',
        challenge_created: 'analytics.challengeCreated', challenge_joined: 'analytics.challengeJoined',
        museum_opened: 'analytics.museumViewed', game_started: 'analytics.gameStarted',
        game_won: 'analytics.gameWon', game_gave_up: 'analytics.gameAbandoned',
        hint_used: 'analytics.hintUsed'
    };
    return labels[value] ? t(labels[value]) : String(value || t('analytics.unknown'));
}

function analyticsMetricCard(label, value, detail = '') {
    return `<div class="analytics-metric">
        <div class="analytics-metric-value">${escapeChallengeHtml(value)}</div>
        <div class="analytics-metric-label">${escapeChallengeHtml(label)}</div>
        ${detail ? `<div class="analytics-metric-detail">${escapeChallengeHtml(detail)}</div>` : ''}
    </div>`;
}

async function showAnalyticsDashboard(days = 30) {
    setAppRoute('/analytics');
    setHeaderControls('analytics');
    const appContent = document.getElementById('app-content');

    if (!isAnalyticsAdmin) {
        appContent.innerHTML = `<div class="game-card empty-state">${t('analytics.restricted')}</div>`;
        return;
    }

    appContent.innerHTML = `<div class="game-card">${renderAppState(t('analytics.loading'))}</div>`;
    const loadingCard = appContent.firstElementChild;
    const analyticsOwnerId = currentUserId;
    const isCurrentRequest = () => appContent.firstElementChild === loadingCard
        && currentUserId === analyticsOwnerId && isAnalyticsAdmin;

    let data;
    try {
        data = await callGameApi('analytics_dashboard', { days });
    } catch (error) {
        if (!isCurrentRequest()) return;
        appContent.innerHTML = `<div class="game-card">${renderAppState(t('analytics.loadError'), {
            type: 'error', detail: error.message
        })}</div>`;
        return;
    }
    if (!isCurrentRequest()) return;

    const summary = data.summary || {};
    const maxStarted = Math.max(1, ...data.byDay.map(day => Number(day.started || 0)));
    const chart = data.byDay.map(day => {
        const height = Math.max(3, Math.round((Number(day.started || 0) / maxStarted) * 100));
        const date = new Date(`${day.date}T00:00:00Z`).toLocaleDateString(
            currentLocale === 'pt-BR' ? 'pt-BR' : currentLocale,
            { month: 'short', day: 'numeric', timeZone: 'UTC' }
        );
        return `<div class="analytics-chart-column" title="${escapeChallengeHtml(t('analytics.daySummary', {
            date, games: day.started, visitors: day.visitors
        }))}">
            <div class="analytics-chart-value">${day.started || ''}</div>
            <div class="analytics-chart-bar" style="height:${height}%"></div>
            <div class="analytics-chart-date">${escapeChallengeHtml(date)}</div>
        </div>`;
    }).join('');

    const difficultyRows = Object.entries(data.byDifficulty || {}).map(([difficulty, values]) => {
        const completionRate = values.started ? Math.round((values.completed / values.started) * 100) : 0;
        return `<div class="analytics-breakdown-row">
            <span>${escapeChallengeHtml(analyticsLabel(difficulty))}</span>
            <strong>${values.started}</strong>
            <span>${t('analytics.completeRate', { rate: completionRate })}</span>
        </div>`;
    }).join('');

    const modeRows = Object.entries(data.byMode || {}).map(([mode, count]) => `
        <div class="analytics-breakdown-row"><span>${escapeChallengeHtml(analyticsLabel(mode))}</span><strong>${count}</strong><span>${t('analytics.sessions')}</span></div>
    `).join('');

    const activityRows = (data.recentActivity || []).map(event => {
        const when = new Date(event.createdAt).toLocaleString(
            currentLocale === 'pt-BR' ? 'pt-BR' : currentLocale
        );
        const context = [
            event.player ? `@${event.player}` : '',
            analyticsLabel(event.mode),
            analyticsLabel(event.difficulty)
        ].filter(value => value && value !== t('analytics.unknown')).join(' · ');
        return `<div class="analytics-activity-row">
            <span>◆</span>
            <div><strong>${escapeChallengeHtml(analyticsLabel(event.type))}</strong>${context ? `<small>${escapeChallengeHtml(context)}</small>` : ''}</div>
            <time>${escapeChallengeHtml(when)}</time>
        </div>`;
    }).join('');

    const playerRows = (data.registeredPlayers || []).map((player, index) => {
        const lastPlayed = player.lastPlayed
            ? new Date(`${player.lastPlayed}T00:00:00`).toLocaleDateString(
                currentLocale === 'pt-BR' ? 'pt-BR' : currentLocale
            )
            : t('common.never');
        return `<div class="analytics-player-row">
            <span class="analytics-player-rank">${index + 1}</span>
            <strong>@${escapeChallengeHtml(player.username)}</strong>
            <span>${t('analytics.playerGames', {
                games: player.gamesPlayed, wins: player.gamesWon, rate: player.winRate
            })}</span>
            <span>${t('analytics.playerStreaks', {
                current: player.currentStreak, best: player.bestStreak
            })}</span>
            <time>${t('analytics.lastDaily', { date: escapeChallengeHtml(lastPlayed) })}</time>
        </div>`;
    }).join('');

    const pagination = data.pagination || {};
    const hasTruncatedData = Object.values(pagination).some(Boolean);

    appContent.innerHTML = `
    <div class="game-card analytics-dashboard">
        <div class="analytics-header">
            <div>
                <div class="friends-kicker">${t('analytics.private')}</div>
                <h2>${t('analytics.title')}</h2>
                <p>${t('analytics.privacyCopy')}</p>
            </div>
            <div class="analytics-range" role="group" aria-label="${t('analytics.period')}">
                ${[7, 30, 90].map(period => `<button class="btn-hint btn-header ${period === data.days ? 'active' : ''}" onclick="showAnalyticsDashboard(${period})">${t('analytics.days', { count: period })}</button>`).join('')}
            </div>
        </div>

        <div class="analytics-metrics">
            ${analyticsMetricCard(t('analytics.uniqueVisitors'), summary.uniqueVisitors, t('analytics.untrackedSessions', { count: summary.untrackedSessions || 0 }))}
            ${analyticsMetricCard(t('analytics.gamesStarted'), summary.totalSessions)}
            ${analyticsMetricCard(t('analytics.gamesCompleted'), summary.completedGames, t('analytics.completion', { rate: summary.completionRate }))}
            ${analyticsMetricCard(t('analytics.wins'), summary.wins, t('analytics.completedWinRate', { rate: summary.winRate }))}
            ${analyticsMetricCard(t('analytics.averageGuesses'), summary.averageGuesses)}
            ${analyticsMetricCard(t('analytics.averageHints'), summary.averageHints)}
            ${analyticsMetricCard(t('analytics.newAccounts'), summary.newAccounts)}
            ${analyticsMetricCard(t('analytics.registeredAccounts'), summary.registeredAccounts, t('analytics.activeAccounts', { count: summary.activeRegisteredPlayers || 0 }))}
            ${analyticsMetricCard(t('analytics.highestStreak'), summary.highestBestStreak, summary.highestStreakPlayer ? `@${summary.highestStreakPlayer}` : t('analytics.noStreak'))}
            ${analyticsMetricCard(t('analytics.anonymousSessions'), summary.anonymousSessions)}
            ${analyticsMetricCard(t('analytics.friendChallenges'), summary.challengesCreated, t('analytics.joins', { count: summary.challengeJoins }))}
            ${analyticsMetricCard(t('analytics.museumViews'), summary.museumViews)}
        </div>

        <section class="analytics-section">
            <h3>${t('analytics.registeredPlayers')}</h3>
            <div class="analytics-players">${playerRows || `<p class="empty-state">${t('analytics.noPlayerStats')}</p>`}</div>
            ${(data.registeredPlayers || []).length >= 100
                ? `<p class="analytics-section-note">${t('analytics.firstAccounts')}</p>`
                : ''}
        </section>

        <section class="analytics-section">
            <h3>${t('analytics.gamesByDay')}</h3>
            <div class="analytics-chart">${chart}</div>
        </section>

        <div class="analytics-two-column">
            <section class="analytics-section">
                <h3>${t('analytics.byLevel')}</h3>
                <div class="analytics-breakdown">${difficultyRows || `<p class="empty-state">${t('analytics.noGames')}</p>`}</div>
            </section>
            <section class="analytics-section">
                <h3>${t('analytics.byMode')}</h3>
                <div class="analytics-breakdown">${modeRows || `<p class="empty-state">${t('analytics.noGames')}</p>`}</div>
            </section>
        </div>

        <section class="analytics-section">
            <h3>${t('analytics.recentActivity')}</h3>
            <div class="analytics-activity">${activityRows || `<p class="empty-state">${t('analytics.noActivity')}</p>`}</div>
        </section>

        ${hasTruncatedData ? `<p class="analytics-data-warning">${t('analytics.partialData')}</p>` : ''}
        <p class="analytics-generated">${t('analytics.generated', {
            date: escapeChallengeHtml(new Date(data.generatedAt).toLocaleString(
                currentLocale === 'pt-BR' ? 'pt-BR' : currentLocale
            ))
        })}</p>
    </div>`;
    focusAppScreenHeading('.analytics-header h2');
}