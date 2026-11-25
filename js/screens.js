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
            <button type="button" class="home-curiosity" onclick="showDailyCuriosity(this)"
                    aria-label="${t('home.curiosity')}" title="${t('home.curiosity')}">?</button>
        </p>
        <p class="difficulty-home-countdown">
        ${t('home.nextDaily')} <span id="countdown-timer">--:--:--</span>
        <span id="countdown-local" class="countdown-local"></span>
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
    void updateHomeStreak(appContent.firstElementChild);
    void refreshDifficultySelectionAccountState();
    // The home screen is what search engines index, so it keeps the full localized title.
    focusAppScreenHeading('.screen-title', { documentTitle: t('app.metaTitle') });
}

function emptyDailyCompletionStatus() {
    return { muito_facil: false, facil: false, normal: false, dificil: false, muito_dificil: false };
}

function getImmediateDailyCompletionStatus() {
    if (!currentUserId) return getGuestDailyCompletionStatus();
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

// A quiet line under the countdown: the length of the player's Daily run, and a nudge
// when today's win is still needed to keep it. Shown from two days on.
async function updateHomeStreak(homeCard) {
    const countdown = homeCard?.querySelector('.difficulty-home-countdown');
    if (!countdown) return;
    let streak;
    try {
        if (currentUserId) {
            const ownerId = currentUserId;
            const { data } = await sb.from('statistics').select('*').eq('user_id', ownerId).maybeSingle();
            if (currentUserId !== ownerId || !homeCard.isConnected) return;
            streak = getEffectiveAccountStreak(data);
        } else {
            streak = getCurrentGuestDailyStreak();
        }
    } catch (error) {
        console.warn('Home streak unavailable:', error);
        return;
    }
    homeCard.querySelector('.home-streak')?.remove();
    if (streak.current < 2) return;
    const line = document.createElement('p');
    line.className = 'home-streak';
    line.innerHTML = `<span class="home-streak-mark" aria-hidden="true">◆</span> `
        + `<strong>${t('result.dayStreak', { count: streak.current })}</strong>`
        + (streak.playedToday ? '' : ` <span class="home-streak-note">· ${t('home.streakKeep')}</span>`);
    countdown.insertAdjacentElement('afterend', line);
}

function readHomeFlag(key) {
    try {
        return localStorage.getItem(key) === '1';
    } catch (_error) {
        return false;
    }
}

function dismissHomeNotice(kind) {
    const key = kind === 'tutorial'
        ? PHYLOSAUR_STORAGE_KEYS.tutorialOfferDismissed
        : PHYLOSAUR_STORAGE_KEYS.guestAccountNoteDismissed;
    try {
        localStorage.setItem(key, '1');
    } catch (_error) {}
    document.querySelector('.guest-account-note')?.remove();
}

// What a guest is told on the home screen, in order of need: someone who has never played
// is offered the guided round; the account is offered only after a first win.
function getHomeNoticeKind() {
    if (currentUserId) return null;
    const results = Object.values(readGuestAchievementProgress().results || {});
    let tutorialDone = false;
    try {
        tutorialDone = localStorage.getItem(PHYLOSAUR_STORAGE_KEYS.tutorialComplete) === 'true';
    } catch (_error) {}
    if (results.length === 0 && !tutorialDone) {
        return readHomeFlag(PHYLOSAUR_STORAGE_KEYS.tutorialOfferDismissed) ? null : 'tutorial';
    }
    if (results.some(result => result?.won)) {
        return readHomeFlag(PHYLOSAUR_STORAGE_KEYS.guestAccountNoteDismissed) ? null : 'account';
    }
    return null;
}

// A quiet, dismissible line under the home actions; only guests see it.
function updateGuestAccountNote(homeCard) {
    const wrap = homeCard?.querySelector('.difficulty-home-actions-wrap');
    if (!wrap) return;
    const kind = getHomeNoticeKind();
    const existing = wrap.querySelector('.guest-account-note');
    if (existing && existing.dataset.kind === kind) return;
    existing?.remove();
    if (!kind) return;
    const isTutorial = kind === 'tutorial';
    const note = document.createElement('div');
    note.className = 'guest-account-note';
    note.dataset.kind = kind;
    note.setAttribute('role', 'note');
    note.innerHTML = `
        <span class="guest-account-note-text">${t(isTutorial ? 'home.tutorialOffer' : 'home.guestNote')}
            <button type="button" class="guest-account-note-action" onclick="${isTutorial ? 'startTutorialGame()' : 'showLoginModal()'}">${t(isTutorial ? 'home.tutorialOfferAction' : 'home.guestNoteAction')}</button>
        </span>
        <button type="button" class="guest-account-note-dismiss" onclick="dismissHomeNotice('${kind}')"
                aria-label="${t('home.guestNoteDismiss')}" title="${t('home.guestNoteDismiss')}">&times;</button>
    `;
    wrap.appendChild(note);
}

let difficultyAccountRefreshGeneration = 0;

async function refreshDifficultySelectionAccountState() {
    if (getCurrentAppRoute() !== '/') return;
    const generation = ++difficultyAccountRefreshGeneration;
    const appContent = document.getElementById('app-content');
    const homeCard = appContent?.firstElementChild;
    // Capture the owner only once the startup account lookup has settled, so a
    // signed-in player is never briefly treated as a guest.
    if (typeof waitForAccountReady === 'function') await waitForAccountReady();
    const ownerId = currentUserId;
    const today = getTodayString();
    const isCurrentRequest = () => generation === difficultyAccountRefreshGeneration
        && getCurrentAppRoute() === '/' && currentUserId === ownerId && getTodayString() === today
        && document.getElementById('app-content') === appContent
        && appContent?.firstElementChild === homeCard;
    if (!homeCard || !document.querySelector('.difficulty-btn[data-difficulty]')) return;
    try {
        const [completionStatus] = await Promise.all([
            getDailyCompletionStatus(),
            initializeAnalyticsAccess()
        ]);
        if (!isCurrentRequest()) return;
        updateDifficultyCompletionStatus(completionStatus);
        if (typeof updateHomeStreak === 'function') void updateHomeStreak(homeCard);
        if (typeof updateGuestAccountNote === 'function') updateGuestAccountNote(homeCard);
        setHeaderControls('difficulty');
    } catch (error) {
        if (!isCurrentRequest()) return;
        console.warn('Daily account state could not be refreshed:', error);
    }
}

function showFriendChallenges(prefilledCode = '') {
    setAppRoute('/friends');
    if (typeof stopChallengeStatusPolling === 'function') stopChallengeStatusPolling();
    setHeaderControls('friends');
    const appContent = document.getElementById('app-content');
    const suggestedName = currentChallengePlayerName || currentUser || '';
    const code = String(prefilledCode || '').toUpperCase().replace(/[^A-Z0-9]/g, '').slice(0, 6);
    const saved = readSavedCustomChallenge();
    const numerals = ['I', 'II', 'III', 'IV', 'V'];

    appContent.innerHTML = `
    <div class="game-card friends-hub">
        <div class="friends-heading">
            <div class="friends-kicker">${t('friends.kicker')}</div>
            <h2 class="screen-title">${t('home.friends')}</h2>
            <p>${t('friends.intro')}</p>
        </div>

        <div class="friend-identity">
            <label class="friend-label" for="challenge-player-name">${t('friends.yourName')}</label>
            <input class="friend-input" id="challenge-player-name" maxlength="24" value="${escapeChallengeHtml(suggestedName)}" placeholder="${t('friends.playerName')}" autocomplete="nickname">
        </div>

        <div class="friends-grid">
            <section class="friend-panel">
                <h3>${t('friends.create')}</h3>
                <fieldset class="challenge-filter">
                    <legend class="friend-label">${t('friends.level')}</legend>
                    <div class="challenge-level-options">
                        ${PHYLOSAUR_LEVELS.map((level, index) => `
                        <label class="challenge-level-option" title="${t(`level.name${index + 1}`)}">
                            <input type="checkbox" name="challenge-level" value="${level}"
                                   aria-label="${t(`level.name${index + 1}`)}"
                                   ${saved.levels.includes(level) ? 'checked' : ''} onchange="updateCustomChallengePool()">
                            <span>${numerals[index]}</span>
                        </label>`).join('')}
                    </div>
                </fieldset>
                <label class="friend-label" for="challenge-clade">${t('friends.clade')}</label>
                <select class="friend-input" id="challenge-clade" onchange="updateCustomChallengePool()">
                    <option value="">${t('friends.allClades')}</option>
                    ${CUSTOM_POOL_CLADES.map(clade => `
                    <option value="${clade}" ${saved.clade === clade ? 'selected' : ''}>${clade}</option>`).join('')}
                </select>
                <input type="hidden" id="challenge-difficulty" value="${buildPoolKey(saved.levels, saved.clade)}">
                <p class="challenge-pool-count" id="challenge-pool-count" aria-live="polite">${t('friends.poolLoading')}</p>

                <div class="challenge-create-actions">
                    <button class="btn-hint friend-action" id="play-solo-btn" onclick="startCustomPractice()">${t('friends.playSolo')}</button>
                    <button class="btn-guess friend-action" id="create-challenge-btn" onclick="createFriendChallenge()">${t('friends.createCode')}</button>
                </div>
            </section>

            <div class="friends-divider" aria-hidden="true"><span>${t('friends.or')}</span></div>

            <section class="friend-panel">
                <h3>${t('friends.join')}</h3>
                <label class="friend-label" for="challenge-code">${t('friends.challengeCode')}</label>
                <input class="friend-input challenge-code-input" id="challenge-code" maxlength="6" value="${escapeChallengeHtml(code)}" placeholder="RAPTOR" autocomplete="off" autocapitalize="characters" spellcheck="false"
                       oninput="this.value=this.value.toUpperCase().replace(/[^A-Z0-9]/g,'').slice(0,6)"
                       onkeydown="if(event.key==='Enter') joinFriendChallenge()">

                <button class="btn-hint friend-action" id="join-challenge-btn" onclick="joinFriendChallenge()">${t('friends.enter')}</button>
            </section>
        </div>

        <p class="friends-note">${t('friends.note')}</p>
    </div>`;

    if (code) document.getElementById('challenge-player-name')?.focus();
    else focusAppScreenHeading();
    void updateCustomChallengePool();
}

function readSavedCustomChallenge() {
    const fallback = { levels: ['normal'], clade: null };
    try {
        const saved = JSON.parse(localStorage.getItem(PHYLOSAUR_STORAGE_KEYS.customChallenge) || 'null');
        const levels = Array.isArray(saved?.levels) ? PHYLOSAUR_LEVELS.filter(level => saved.levels.includes(level)) : [];
        return levels.length
            ? { levels, clade: CUSTOM_POOL_CLADES.includes(saved?.clade) ? saved.clade : null }
            : fallback;
    } catch (error) {
        return fallback;
    }
}

let customChallengeCatalogPromise = null;

// The same catalog as the Museum (level and lineage of every active genus), so the
// screen can tell how many genera a filter allows before anything is created.
function loadCustomChallengeCatalog() {
    if (fullDatabase.length > 0 && fullDatabase.every(dino => dino.dificuldade && Array.isArray(dino.linhagem))) {
        return Promise.resolve(fullDatabase);
    }
    customChallengeCatalogPromise ??= callGameApi('catalog')
        .then(catalog => ensureMuseumCatalogLineages(catalog.dinosaurs || []))
        .catch(error => {
            customChallengeCatalogPromise = null;
            throw error;
        });
    return customChallengeCatalogPromise;
}

let customChallengePoolGeneration = 0;

async function updateCustomChallengePool() {
    const levels = [...document.querySelectorAll('input[name="challenge-level"]:checked')].map(input => input.value);
    const clade = document.getElementById('challenge-clade')?.value || null;
    const poolInput = document.getElementById('challenge-difficulty');
    const status = document.getElementById('challenge-pool-count');
    if (!poolInput || !status) return;
    const poolKey = buildPoolKey(levels, clade);
    poolInput.value = poolKey;
    try {
        localStorage.setItem(PHYLOSAUR_STORAGE_KEYS.customChallenge, JSON.stringify({ levels, clade }));
    } catch (error) {
        // Remembering the filters is only a convenience.
    }

    const generation = ++customChallengePoolGeneration;
    const show = (message, blocked) => {
        status.textContent = message;
        status.classList.toggle('challenge-pool-warning', blocked);
        poolInput.dataset.poolBlocked = blocked ? 'true' : 'false';
    };
    if (!poolKey) {
        show(t('friends.chooseLevel'), true);
        return;
    }

    try {
        const catalog = await loadCustomChallengeCatalog();
        if (generation !== customChallengePoolGeneration || !status.isConnected) return;
        const count = catalog.filter(dino => levels.includes(dino.dificuldade)
            && (!clade || (Array.isArray(dino.linhagem) && dino.linhagem.includes(clade)))).length;
        const tooSmall = Boolean(parseCustomPoolKey(poolKey)) && count < MINIMUM_CUSTOM_POOL_SIZE;
        show(tooSmall
            ? t('friends.poolTooSmall', { count, minimum: MINIMUM_CUSTOM_POOL_SIZE })
            : t('friends.poolCount', { count }), tooSmall);
    } catch (error) {
        // Without the catalog the count is unknown; the server still validates the pool.
        if (generation !== customChallengePoolGeneration || !status.isConnected) return;
        console.warn('Challenge pool could not be counted:', error);
        show(t('friends.poolUnknown'), false);
    }
}

async function startCustomPractice() {
    const poolInput = document.getElementById('challenge-difficulty');
    const poolKey = poolInput?.value || '';
    if (!poolKey || poolInput.dataset.poolBlocked === 'true') {
        await customAlert(t('friends.create'),
            document.getElementById('challenge-pool-count')?.textContent || t('friends.chooseLevel'));
        return;
    }
    await startPracticeChallenge(poolKey);
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
    const nameInput = document.getElementById('challenge-player-name');
    const difficultyInput = document.getElementById('challenge-difficulty');
    const button = document.getElementById('create-challenge-btn');
    const playerName = nameInput?.value.trim().slice(0, 24) || t('common.player');
    if (!difficultyInput?.value || difficultyInput.dataset?.poolBlocked === 'true') {
        await customAlert(t('friends.create'),
            document.getElementById('challenge-pool-count')?.textContent || t('friends.chooseLevel'));
        return;
    }
    const request = beginFriendChallengeRequest();
    if (!request) return;

    button.textContent = t('friends.creating');
    try {
        const data = await callGameApi('create_challenge', {
            difficulty: difficultyInput.value,
            playerName
        });
        if (!request.isCurrent()) return;
        // The race exists on the server already: failing to remember the seat
        // (blocked storage) must not report a failure or invite a duplicate.
        try { localStorage.setItem(getChallengeSessionStorageKey(data.challenge.code), data.sessionId); } catch (_error) {}
        await startFriendChallengeFromPayload(data);
    } catch (error) {
        if (!request.isCurrent()) return;
        await customAlert(t('friends.createError'), escapeHtml(error.message));
    } finally {
        request.finish();
    }
}

async function joinFriendChallenge() {
    const nameInput = document.getElementById('challenge-player-name');
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
        await customAlert(t('stats.title'), t('stats.loginRequired'));
        showLoginModal();
        return;
    }
    setAppRoute('/stats');
    setHeaderControls('stats');
    const appContent = document.getElementById('app-content');
    appContent.innerHTML = `<div class="game-card stats-dashboard">${renderAppState(t('stats.loading'))}</div>`;
    let loadingCard = appContent.firstElementChild;
    const statsOwnerId = currentUserId;
    const isCurrentRequest = () => appContent.firstElementChild === loadingCard
        && currentUserId === statsOwnerId && Boolean(currentUser);

    let results;
    try {
        results = await Promise.all([
        sb.from('statistics').select('*').eq('user_id', statsOwnerId).single(),
        fetchAllDailyResults('played_date, difficulty, guess_count, hint_history, won', statsOwnerId),
        sb.from('daily_results').select('*').eq('user_id', statsOwnerId)
            .order('created_at', { ascending: false }).limit(10),
        sb.from('achievements').select('achievement_id').eq('user_id', statsOwnerId)
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
    const [statsResult, dailyResult, recentGamesResult, achievementsResult] = results;
    const stats = statsResult.data;
    const dailyRows = dailyResult.data || [];
    const difficultyHistory = dailyRows;
    const achievementHistory = dailyRows.filter(result => result.won === true);
    const recentGames = recentGamesResult.data;
    const achievements = achievementsResult.data;

    const gamesPlayed = stats?.games_played || 0;
    const gamesWon = stats?.games_won || 0;
    const winRate = gamesPlayed > 0 ? Math.round((gamesWon / gamesPlayed) * 100) : 0;
    const summary = computeStatsSummary(difficultyHistory, achievementHistory);
    // Same "is it still alive" rule as the home screen, so the two never disagree.
    const streakData = { current: typeof getEffectiveAccountStreak === 'function'
            ? getEffectiveAccountStreak(stats).current : (stats?.current_streak || 0), best: stats?.best_streak || 0, lastPlayed: stats?.last_played };
    let unlockedAchievements = new Set(achievements ? achievements.map(a => a.achievement_id) : []);
    let supplementalAchievementProgress = {};

    // The panel is drawn first from the saved achievements and refreshed in place
    // once the two synchronizations below finish, so slow network calls never
    // keep the whole screen waiting.
    const renderAchievementsPanel = (unlockedSet, supplementalProgress) => {
        const unlockedAchievementCount = ACHIEVEMENT_DEFINITIONS
            .filter(achievement => unlockedSet.has(achievement.id)).length;
        const progressById = buildAchievementProgress(stats, achievementHistory, supplementalProgress);
        const completion = ACHIEVEMENT_DEFINITIONS.length
            ? Math.round((unlockedAchievementCount / ACHIEVEMENT_DEFINITIONS.length) * 100) : 0;
        // A summary first (overall progress and the few closest to unlocking); the whole
        // collection stays one tap away instead of filling half of the page.
        const closest = typeof pickClosestAchievements === 'function'
            ? pickClosestAchievements(unlockedSet, progressById, 4) : [];
        return `
        <div class="achievements-heading">
            <h3>${t('stats.achievements')}</h3>
            <span>${t('stats.unlockedCount', { count: unlockedAchievementCount, total: ACHIEVEMENT_DEFINITIONS.length })}</span>
        </div>
        <div class="achievements-overall" role="img" aria-label="${completion}%">
            <div class="achievement-progress-track"><div class="achievement-progress-fill" style="width:${completion}%;"></div></div>
        </div>
        ${closest.length ? `
        <p class="stats-section-kicker achievements-closest-title">${t('stats.closestTitle')}</p>
        ${generateAchievements(unlockedSet, progressById, closest)}` : ''}
        <details class="achievements-all">
            <summary>${t('stats.showAllAchievements', { total: ACHIEVEMENT_DEFINITIONS.length })}</summary>
            ${generateAchievements(unlockedSet, progressById)}
        </details>`;
    };

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
            <div class="stat"><div class="stat-value">${summary.averagePerWin}</div><div class="stat-label">${t('stats.averagePerWin')}</div></div>
            <div class="stat"><div class="stat-value">${summary.noHintWins}</div><div class="stat-label">${t('stats.noHintWins')}</div></div>
        </div>

        <div class="stats-insights-grid">
            ${generateStreakDisplay(streakData)}
            ${typeof generateActivityHeatmap === 'function' ? generateActivityHeatmap(dailyRows) : ''}
        </div>

        <div class="stats-charts-grid">
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
            ${typeof generateTrendChart === 'function' ? generateTrendChart(achievementHistory) : ''}
        </div>

        <div class="stats-section">
            <div class="stats-section-heading">
                <div>
                    <p class="stats-section-kicker">${t('stats.difficultyProfile')}</p>
                    <h3 class="stats-section-title">${t('stats.performanceLevel')}</h3>
                </div>
                ${renderStatsLevelChips(summary)}
            </div>
            ${generateDifficultyStats(difficultyHistory)}
        </div>

        <div class="achievements-panel" data-stats-achievements>${renderAchievementsPanel(unlockedAchievements, supplementalAchievementProgress)}</div>

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
    loadingCard = appContent.firstElementChild;

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

    const achievementsPanel = appContent.firstElementChild?.querySelector?.('[data-stats-achievements]');
    if (achievementsPanel) {
        // Refreshing in place must not close the list the player may already have opened.
        const listWasOpen = Boolean(achievementsPanel.querySelector?.('details.achievements-all')?.open);
        achievementsPanel.innerHTML = renderAchievementsPanel(unlockedAchievements, supplementalAchievementProgress);
        if (listWasOpen) achievementsPanel.querySelector?.('details.achievements-all')?.setAttribute('open', '');
    }
    focusAppScreenHeading();
}

// Reads every Daily result in pages: one request returns at most 1,000 rows, which a
// long-time player's history will eventually exceed.
function fetchAllDailyResults(columns, ownerId) {
    const pageSize = 1000;
    const rows = [];
    const fetchPage = from => sb.from('daily_results').select(columns).eq('user_id', ownerId)
        .order('played_date', { ascending: true }).order('difficulty', { ascending: true })
        .range(from, from + pageSize - 1)
        .then(page => {
            if (page.error) return { data: null, error: page.error };
            const chunk = page.data || [];
            rows.push(...chunk);
            return chunk.length < pageSize ? { data: rows, error: null } : fetchPage(from + pageSize);
        });
    return fetchPage(0);
}

// Numbers that sit next to the headline totals: average guesses per win, wins without
// hints, and the most played / hardest won level.
function computeStatsSummary(difficultyHistory, wonResults) {
    const levels = ['muito_facil', 'facil', 'normal', 'dificil', 'muito_dificil'];
    const localeTag = typeof getPhylosaurLocaleTag === 'function' ? getPhylosaurLocaleTag() : undefined;
    const wins = (wonResults || []).filter(result => Number(result.guess_count) > 0);
    const averagePerWin = wins.length
        ? (wins.reduce((sum, result) => sum + Number(result.guess_count), 0) / wins.length)
            .toLocaleString(localeTag, { maximumFractionDigits: 1 })
        : '-';
    const noHintWins = (wonResults || []).filter(result =>
        Array.isArray(result.hint_history) && result.hint_history.length === 0).length;

    const playedByLevel = new Map();
    let hardestWon = -1;
    (difficultyHistory || []).forEach(result => {
        const index = levels.indexOf(result.difficulty);
        if (index < 0) return;
        playedByLevel.set(result.difficulty, (playedByLevel.get(result.difficulty) || 0) + 1);
        if (result.won === true) hardestWon = Math.max(hardestWon, index);
    });
    const favorite = [...playedByLevel].sort((first, second) => second[1] - first[1]
        || levels.indexOf(second[0]) - levels.indexOf(first[0]))[0]?.[0];

    return {
        averagePerWin,
        noHintWins,
        favoriteLevel: favorite || null,
        hardestWinLevel: hardestWon >= 0 ? levels[hardestWon] : null,
    };
}

// "Most played" and "hardest won" level, as two small labelled chips.
function renderStatsLevelChips(summary) {
    const levels = ['muito_facil', 'facil', 'normal', 'dificil', 'muito_dificil'];
    const levelName = difficulty => t(`level.name${levels.indexOf(difficulty) + 1}`);
    if (!summary || (!summary.favoriteLevel && !summary.hardestWinLevel)) return '';
    return `
        <div class="stats-chips">
            ${summary.favoriteLevel ? `<span class="stats-chip">${t('stats.favoriteLevel')} <strong>${escapeChallengeHtml(levelName(summary.favoriteLevel))}</strong></span>` : ''}
            ${summary.hardestWinLevel ? `<span class="stats-chip">${t('stats.hardestWin')} <strong>${escapeChallengeHtml(levelName(summary.hardestWinLevel))}</strong></span>` : ''}
        </div>`;
}

function formatStatsDate(value) {
    if (!value) return t('common.never');
    const date = /^\d{4}-\d{2}-\d{2}$/.test(String(value))
        ? new Date(`${value}T00:00:00Z`)
        : new Date(value);
    if (Number.isNaN(date.getTime())) return String(value);
    const localeTag = typeof getPhylosaurLocaleTag === 'function' ? getPhylosaurLocaleTag() : undefined;
    return date.toLocaleDateString(localeTag, { year: 'numeric', month: 'long', day: 'numeric', timeZone: 'UTC' });
}

function generateStreakDisplay(streakData) {
    if (!currentUser) return '';

    const best = Number(streakData?.best) || 0;
    const bestLine = best > 0
        ? `<div class="streak-copy">${t(best === 1 ? 'stats.bestDayOne' : 'stats.bestDayMany', { count: best })}</div>`
        : '';
    const lastPlayedLine = streakData?.lastPlayed
        ? `<div class="streak-meta">${t('stats.lastPlayed', { date: formatStatsDate(streakData.lastPlayed) })}</div>`
        : '';

    if (!streakData || streakData.current === 0) {
    return `
        <div class="streak-card streak-card-empty">
        <div class="streak-empty-title">${t('stats.noStreak')}</div>
        <div class="streak-copy">${t(best > 0 && streakData?.lastPlayed ? 'stats.streakEnded' : 'stats.noStreakCopy')}</div>
        ${bestLine}
        ${lastPlayedLine}
        </div>
    `;
    }

    return `
    <div class="streak-card streak-card-active">
        <div class="streak-value">◆ ${streakData.current}</div>
        <div class="streak-heading">${t('stats.currentStreak')}</div>
        ${bestLine}
        <div class="streak-meta">${t('stats.lastPlayed', { date: formatStatsDate(streakData.lastPlayed) })}</div>
    </div>
    `;
}

// ── Activity calendar ────────────────────────────────────────────────────────
// One cell per day for the last 26 weeks, Monday-first columns: faint = no game,
// red = games but no win, gold = a win, bright gold = two or more wins.
function generateActivityHeatmap(dailyRows) {
    // A phone shows fewer weeks so each day stays big enough to see (and tap).
    const WEEKS = globalThis.matchMedia?.('(max-width: 600px)').matches ? 17 : 26;
    const DAY = 86400000;
    const todayText = getTodayString();
    const today = Date.parse(`${todayText}T00:00:00Z`);
    const mondayIndex = time => (new Date(time).getUTCDay() + 6) % 7;
    const start = today - ((WEEKS - 1) * 7 + mondayIndex(today)) * DAY;

    const byDay = new Map();
    (dailyRows || []).forEach(row => {
        const date = String(row.played_date || '');
        if (!/^\d{4}-\d{2}-\d{2}$/.test(date)) return;
        const entry = byDay.get(date) || { played: 0, won: 0 };
        entry.played += 1;
        if (row.won === true) entry.won += 1;
        byDay.set(date, entry);
    });

    const localeTag = typeof getPhylosaurLocaleTag === 'function' ? getPhylosaurLocaleTag() : undefined;
    const dayLabel = time => new Date(time).toLocaleDateString(localeTag, { day: 'numeric', month: 'short', timeZone: 'UTC' });
    const monthLabel = time => new Date(time).toLocaleDateString(localeTag, { month: 'short', timeZone: 'UTC' });

    let cells = '';
    let monthLabels = '';
    let daysPlayed = 0;
    let previousMonth = -1;
    for (let week = 0; week < WEEKS; week++) {
        const weekStart = start + week * 7 * DAY;
        const month = new Date(weekStart).getUTCMonth();
        if (month !== previousMonth && (week === 0 || new Date(weekStart).getUTCDate() <= 7)) {
            monthLabels += `<span style="grid-column:${week + 1}">${escapeChallengeHtml(monthLabel(weekStart))}</span>`;
            previousMonth = month;
        }
        for (let weekday = 0; weekday < 7; weekday++) {
            const time = weekStart + weekday * DAY;
            const date = new Date(time).toISOString().slice(0, 10);
            if (time > today) { cells += '<i class="activity-cell is-future"></i>'; continue; }
            const entry = byDay.get(date);
            if (entry) daysPlayed += 1;
            const state = !entry ? 'is-none' : entry.won === 0 ? 'is-lost' : entry.won > 1 ? 'is-won-many' : 'is-won';
            const title = entry
                ? `${dayLabel(time)} · ${t('stats.recordWon', { won: entry.won, played: entry.played })}`
                : dayLabel(time);
            cells += `<i class="activity-cell ${state}" title="${escapeChallengeHtml(title)}"></i>`;
        }
    }

    return `
        <section class="stats-section stats-activity-section" aria-labelledby="activity-title">
            <div class="stats-section-heading">
                <div>
                    <p class="stats-section-kicker">${t('stats.activityKicker')}</p>
                    <h3 class="stats-section-title" id="activity-title">${t('stats.activityTitle', { weeks: WEEKS })}</h3>
                </div>
                <span>${t('stats.activityDays', { count: daysPlayed })}</span>
            </div>
            <div class="activity-months" style="--activity-weeks:${WEEKS}" aria-hidden="true">${monthLabels}</div>
            <div class="activity-grid" style="--activity-weeks:${WEEKS}" role="img" aria-label="${t('stats.activityDays', { count: daysPlayed })}">${cells}</div>
            <div class="activity-legend" aria-hidden="true">
                <span><i class="activity-cell is-none"></i>${t('stats.legendNone')}</span>
                <span><i class="activity-cell is-lost"></i>${t('stats.legendLost')}</span>
                <span><i class="activity-cell is-won"></i>${t('stats.legendWon')}</span>
                <span><i class="activity-cell is-won-many"></i>${t('stats.legendWonMany')}</span>
            </div>
        </section>`;
}

// ── Progress over time ───────────────────────────────────────────────────────
// Average guesses per winning game, one point per week (lower is better), plus a
// comparison of the last four weeks with the weeks before them.
function generateTrendChart(wonRows) {
    const DAY = 86400000;
    const weekStartOf = date => {
        const time = Date.parse(`${date}T00:00:00Z`);
        return time - ((new Date(time).getUTCDay() + 6) % 7) * DAY;
    };
    const weeks = new Map();
    (wonRows || []).forEach(row => {
        const date = String(row.played_date || '');
        const guesses = Number(row.guess_count);
        if (!/^\d{4}-\d{2}-\d{2}$/.test(date) || !Number.isFinite(guesses) || guesses < 1) return;
        const key = weekStartOf(date);
        const entry = weeks.get(key) || { sum: 0, count: 0 };
        entry.sum += guesses;
        entry.count += 1;
        weeks.set(key, entry);
    });
    const points = [...weeks].sort((a, b) => a[0] - b[0]).slice(-12)
        .map(([time, entry]) => ({ time, average: entry.sum / entry.count, count: entry.count }));

    const heading = `
            <div class="stats-section-heading">
                <div>
                    <p class="stats-section-kicker">${t('stats.trendKicker')}</p>
                    <h3 class="stats-section-title">${t('stats.trendTitle')}</h3>
                </div>
                <span>${t('stats.trendLowerBetter')}</span>
            </div>`;
    if (points.length < 2) {
        return `<section class="stats-section stats-trend-section">${heading}
            <p class="empty-stats">${t('stats.trendEmpty')}</p></section>`;
    }

    const localeTag = typeof getPhylosaurLocaleTag === 'function' ? getPhylosaurLocaleTag() : undefined;
    const format = value => value.toLocaleString(localeTag, { maximumFractionDigits: 1 });
    const W = 400, H = 170, left = 34, right = 12, top = 14, bottom = 28;
    const values = points.map(point => point.average);
    const low = Math.max(1, Math.floor(Math.min(...values) - 0.5));
    const high = Math.max(low + 2, Math.ceil(Math.max(...values) + 0.5));
    const first = points[0].time;
    const span = Math.max(1, points[points.length - 1].time - first);
    const x = time => left + ((time - first) / span) * (W - left - right);
    const y = value => top + (1 - (value - low) / (high - low)) * (H - top - bottom);
    const line = points.map((point, index) => `${index ? 'L' : 'M'}${x(point.time).toFixed(1)} ${y(point.average).toFixed(1)}`).join(' ');
    const area = `${line} L${x(points[points.length - 1].time).toFixed(1)} ${H - bottom} L${x(first).toFixed(1)} ${H - bottom} Z`;
    const ticks = [low, (low + high) / 2, high];
    const dayLabel = time => new Date(time).toLocaleDateString(localeTag, { day: 'numeric', month: 'short', timeZone: 'UTC' });

    const recent = points.slice(-4);
    const earlier = points.slice(0, -4);
    const mean = list => list.reduce((sum, point) => sum + point.average * point.count, 0)
        / list.reduce((sum, point) => sum + point.count, 0);
    let comparison = '';
    if (earlier.length) {
        const nowValue = mean(recent);
        const beforeValue = mean(earlier);
        const trend = nowValue < beforeValue - 0.05 ? 'is-better' : nowValue > beforeValue + 0.05 ? 'is-worse' : 'is-flat';
        const arrow = trend === 'is-better' ? '▼' : trend === 'is-worse' ? '▲' : '■';
        comparison = `<p class="trend-compare ${trend}"><span aria-hidden="true">${arrow}</span> ${t('stats.trendRecent')} <strong>${format(nowValue)}</strong> · ${t('stats.trendBefore')} <strong>${format(beforeValue)}</strong></p>`;
    }

    return `
        <section class="stats-section stats-trend-section">${heading}
            <svg class="trend-chart" viewBox="0 0 ${W} ${H}" role="img"
                 aria-label="${t('stats.trendTitle')}: ${points.map(point => format(point.average)).join(', ')}">
                ${ticks.map(tick => `
                <line class="trend-grid" x1="${left}" x2="${W - right}" y1="${y(tick).toFixed(1)}" y2="${y(tick).toFixed(1)}"/>
                <text class="trend-axis" x="${left - 6}" y="${(y(tick) + 4).toFixed(1)}" text-anchor="end">${format(tick)}</text>`).join('')}
                <path class="trend-area" d="${area}"/>
                <path class="trend-line" d="${line}"/>
                ${points.map(point => `<circle class="trend-dot" cx="${x(point.time).toFixed(1)}" cy="${y(point.average).toFixed(1)}" r="3.4"><title>${escapeChallengeHtml(`${dayLabel(point.time)} · ${format(point.average)}`)}</title></circle>`).join('')}
                <text class="trend-axis" x="${left}" y="${H - 8}" text-anchor="start">${escapeChallengeHtml(dayLabel(first))}</text>
                <text class="trend-axis" x="${W - right}" y="${H - 8}" text-anchor="end">${escapeChallengeHtml(dayLabel(points[points.length - 1].time))}</text>
            </svg>
            ${comparison}
        </section>`;
}

// The few achievements that are closest to being unlocked (highest progress first).
function pickClosestAchievements(unlockedSet, progressById, limit = 4) {
    return ACHIEVEMENT_DEFINITIONS
        .filter(achievement => !unlockedSet.has(achievement.id))
        .map(achievement => {
            const progress = progressById?.[achievement.id];
            const ratio = progress && progress.target > 0 ? Math.min(1, progress.current / progress.target) : 0;
            return { id: achievement.id, ratio };
        })
        .sort((first, second) => second.ratio - first.ratio)
        .slice(0, limit)
        .map(item => item.id);
}

let mathRendererPromise = null;

function loadPhylosaurScript(src) {
    const existing = document.querySelector(`script[src="${src}"]`);
    if (existing?.dataset.loaded === 'true') return Promise.resolve();
    if (existing?.phylosaurLoadPromise) return existing.phylosaurLoadPromise;
    const script = existing || document.createElement('script');
    script.src = src;
    script.phylosaurLoadPromise = new Promise((resolve, reject) => {
        const cleanup = () => {
            clearTimeout(timeout);
            script.removeEventListener('load', onLoad);
            script.removeEventListener('error', onError);
            script.phylosaurLoadPromise = null;
        };
        const onLoad = () => {
            script.dataset.loaded = 'true';
            cleanup();
            resolve();
        };
        const onError = () => {
            cleanup();
            script.remove();
            reject(new Error(`Could not load script: ${src}`));
        };
        const timeout = setTimeout(onError, 15000);
        script.addEventListener('load', onLoad, { once: true });
        script.addEventListener('error', onError, { once: true });
        if (!existing) document.head.appendChild(script);
    });
    return script.phylosaurLoadPromise;
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
    let aboutCard = appContent.firstElementChild;
    const isCurrentRequest = () => getCurrentAppRoute() === '/about'
        && appContent.firstElementChild === aboutCard;

    try {
        const response = await fetch('about.html');
        if (!isCurrentRequest()) return;
        if (!response.ok) throw new Error(`About page returned ${response.status}.`);
        const articleHtml = await response.text();
        if (!isCurrentRequest()) return;
        const aboutDocument = new DOMParser().parseFromString(articleHtml, 'text/html');
        const article = aboutDocument.querySelector('#about-content article');
        if (!article) throw new Error('About content is unavailable.');
        article.querySelector('h2')?.remove();
        article.querySelectorAll('script').forEach(script => script.remove());
        if (!isCurrentRequest()) return;

        appContent.innerHTML = `
            <div class="game-card about-screen">
                <h2 class="screen-title about-screen-title">${t('about.title')}</h2>
                <div class="about-screen-body">${article.innerHTML}</div>
                <button class="btn-new-game" onclick="navigateToAppRoute('/')">${t('about.return')}</button>
            </div>`;
        aboutCard = appContent.firstElementChild;
        focusAppScreenHeading();
    } catch (error) {
        if (!isCurrentRequest()) return;
        console.warn('About page could not be loaded:', error);
        appContent.innerHTML = `<div class="game-card about-screen">${renderAppState(
            t('about.error'), { type: 'error' }
        )}<button class="btn-new-game" onclick="navigateToAppRoute('/')">${t('about.return')}</button></div>`;
        return;
    }

    // Formula rendering is optional: a CDN failure must not hide the article.
    try {
        await ensureMathRenderer();
        if (!isCurrentRequest() || !aboutCard?.isConnected || !window.renderMathInElement) return;
        window.renderMathInElement(aboutCard, {
            delimiters: [
                { left: '$$', right: '$$', display: true },
                { left: '$', right: '$', display: false }
            ],
            throwOnError: false
        });
    } catch (error) {
        if (!isCurrentRequest()) return;
        console.warn('About formula rendering is unavailable:', error);
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

function markFirstRunTutorialComplete() {
    try {
        localStorage.setItem(FIRST_RUN_TUTORIAL_KEY, 'true');
    } catch (error) {
        console.warn('Could not save tutorial preference:', error);
    }
}

function showInteractiveTutorial() {
    return startTutorialGame();
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
        winningGuesses: 0
    }]));

    for (const result of difficultyHistory || []) {
        const stat = statsByDifficulty.get(result.difficulty);
        if (!stat) continue;
        stat.played += 1;
        if (result.won === true) {
            stat.won += 1;
            stat.winningGuesses += Math.max(0, Number(result.guess_count || 0));
        }
    }

    const localeTag = typeof getPhylosaurLocaleTag === 'function' ? getPhylosaurLocaleTag() : undefined;
    return `<div class="difficulty-stats-list">${difficultyOrder.map(difficulty => {
        const stat = statsByDifficulty.get(difficulty);
        const played = stat.played;
        const won = stat.won;
        const winRate = played > 0 ? Math.round((won / played) * 100) : 0;
        const average = won > 0 ? Math.round((stat.winningGuesses / won) * 10) / 10 : 0;
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
                    <span class="diff-avg">${won
                        ? t('stats.averageGuesses', {
                            count: average.toLocaleString(localeTag, { maximumFractionDigits: 1 }),
                            unit: t(average === 1 ? 'game.guessOne' : 'game.guessMany')
                        })
                        : t('stats.noAverage')}</span>
                </div>
            </article>
        `;
    }).join('')}</div>`;
}

function generateAchievements(unlockedSet, progressById = {}, onlyIds = null) {
    const allAchievements = onlyIds
        ? ACHIEVEMENT_DEFINITIONS.filter(achievement => onlyIds.includes(achievement.id))
        : ACHIEVEMENT_DEFINITIONS;

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

let museumUnlockedOnly = false;
// The search box keeps what the player typed; filtering uses museumSearchQuery.
let museumSearchText = '';

// Gallery filters survive a reload in the same tab, but not a new visit.
function saveMuseumFilters() {
    try {
        sessionStorage.setItem(PHYLOSAUR_STORAGE_KEYS.museumFilters, JSON.stringify({
            level: selectedMuseumLevel,
            clade: selectedMuseumClade,
            search: museumSearchText,
            unlockedOnly: museumUnlockedOnly
        }));
    } catch (_error) {}
}

(function restoreMuseumFilters() {
    let saved = null;
    try {
        saved = JSON.parse(sessionStorage.getItem(PHYLOSAUR_STORAGE_KEYS.museumFilters) || 'null');
    } catch (_error) {}
    if (!saved) return;
    if (['all', ...PHYLOSAUR_LEVELS].includes(saved.level)) selectedMuseumLevel = saved.level;
    if (saved.clade === 'all' || saved.clade === 'other'
        || MUSEUM_ATLAS_COLLECTIONS.some(definition => definition.clade === saved.clade)) {
        selectedMuseumClade = saved.clade;
    }
    if (typeof saved.search === 'string') {
        museumSearchText = saved.search.slice(0, 80);
        museumSearchQuery = museumSearchText.trim().toLowerCase();
    }
    museumUnlockedOnly = saved.unlockedOnly === true;
})();

let museumOverrideCatalogPromise = null;
const museumCommonsOverrideRequests = new Map();
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
        museumLineageCatalogPromise = fetch('phylosaur_db.json?v=atlas-3')
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
        museumPaleodataCatalogPromise = fetch('phylosaur_paleodata.json?v=11')
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
                museumPaleodataCatalogPromise = null;
                return { timeline: { oldest_ma: 251.9, youngest_ma: 66 }, taxa: {} };
            });
    }

    return museumPaleodataCatalogPromise;
}

function formatMuseumAge(value) {
    const age = Number(value);
    if (!Number.isFinite(age)) return '';
    return Number.isInteger(age)
        ? String(age)
        : age.toLocaleString(getPhylosaurLocaleTag(), { minimumFractionDigits: 1, maximumFractionDigits: 1 });
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
    'Albian': 'museum.ageAlbian',
    'Aptian': 'museum.ageAptian',
    'Barremian–Early Aptian': 'museum.ageBarremianEarlyAptian',
    'Campanian': 'museum.ageCampanian',
    'Cenomanian': 'museum.ageCenomanian',
    'Late Carnian': 'museum.ageLateCarnian',
    'Late Maastrichtian': 'museum.ageLateMaastrichtian',
    'Norian': 'museum.ageNorian',
    'Barremian': 'museum.ageBarremian',
    'Barremian–Aptian': 'museum.ageBarremianAptian',
    'Campanian–Maastrichtian': 'museum.ageCampanianMaastrichtian',
    'Late Campanian–Maastrichtian': 'museum.ageLateCampanianMaastrichtian',
    'Kimmeridgian': 'museum.ageKimmeridgian',
    'Oxfordian': 'museum.ageOxfordian',
    'Sinemurian': 'museum.ageSinemurian',
    'Berriasian–Early Hauterivian': 'museum.ageBerriasianEarlyHauterivian',
    'Berriasian–Valanginian': 'museum.ageBerriasianValanginian',
    'Coniacian–Santonian': 'museum.ageConiacianSantonian',
    'Early Campanian': 'museum.ageEarlyCampanian',
    'Late Albian': 'museum.ageLateAlbian',
    'Late Barremian–Early Aptian': 'museum.ageLateBarremianEarlyAptian',
    'Late Turonian': 'museum.ageLateTuronian',
    'Santonian': 'museum.ageSantonian',
    'Turonian': 'museum.ageTuronian',
    'Turonian–Coniacian': 'museum.ageTuronianConiacian',
    'Late Turonian–Early Coniacian': 'museum.ageLateTuronianEarlyConiacian',
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
    'Morocco': 'museum.placeMorocco',
    'Belgium': 'museum.placeBelgium',
    'Brazil': 'museum.placeBrazil',
    'Egypt': 'museum.placeEgypt',
    'Russia': 'museum.placeRussia',
    'Switzerland': 'museum.placeSwitzerland',
    'Tanzania': 'museum.placeTanzania',
    'Lesotho': 'museum.placeLesotho',
    'Niger': 'museum.placeNiger',
    'South Africa': 'museum.placeSouthAfrica',
    'Zimbabwe': 'museum.placeZimbabwe',
    'India': 'museum.placeIndia',
    'Angola': 'museum.placeAngola',
    'Australia': 'museum.placeAustralia',
    'Japan': 'museum.placeJapan',
    'Laos': 'museum.placeLaos',
    'Malawi': 'museum.placeMalawi',
    'Mexico': 'museum.placeMexico',
    'Oceania': 'museum.placeOceania',
    'Thailand': 'museum.placeThailand',
    'Uzbekistan': 'museum.placeUzbekistan'
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
        : hasAgeRange && maxMa === minMa
            ? t('museum.millionYearsAgoSingle', { age: formatMuseumAge(maxMa) })
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
        museumOverrideCatalogPromise = fetch('phylosaur_media_overrides.json?v=43')
            .then(response => {
                if (!response.ok) throw new Error(`Media overrides HTTP ${response.status}`);
                return response.json();
            })
            .then(catalog => catalog.taxa || {})
            .catch(error => {
                console.warn('Museum media overrides unavailable:', error);
                museumOverrideCatalogPromise = null;
                return {};
            });
    }

    return museumOverrideCatalogPromise;
}

async function loadMuseumFallbackCatalog() {
    if (!museumFallbackCatalogPromise) {
        museumFallbackCatalogPromise = fetch('phylosaur_media_fallback.json?v=2')
            .then(response => {
                if (!response.ok) throw new Error(`Media catalog HTTP ${response.status}`);
                return response.json();
            })
            .then(catalog => catalog.taxa || {})
            .catch(error => {
                console.warn('Museum fallback catalog unavailable:', error);
                museumFallbackCatalogPromise = null;
                return {};
            });
    }

    return museumFallbackCatalogPromise;
}

async function resolveMuseumCommonsOverride(override) {
    const title = override.file_title;
    if (museumCommonsOverrideRequests.has(title)) return museumCommonsOverrideRequests.get(title);
    const request = (async () => {
        const controller = new AbortController();
        const timeout = setTimeout(() => controller.abort(), 8000);
        try {
            const parameters = new URLSearchParams({
                action: 'query', titles: title, prop: 'imageinfo',
                iiprop: 'url|extmetadata', iiurlwidth: '960',
                format: 'json', origin: '*'
            });
            const response = await fetch(`https://commons.wikimedia.org/w/api.php?${parameters}`, {
                signal: controller.signal
            });
            if (!response.ok) throw new Error(`Commons image metadata HTTP ${response.status}`);
            const data = await response.json();
            const info = Object.values(data.query?.pages || {})
                .find(page => page.imageinfo?.length)?.imageinfo[0];
            const metadata = info?.extmetadata || {};
            const plainText = value => new DOMParser()
                .parseFromString(String(value || ''), 'text/html').body.textContent.trim();
            const artist = plainText(metadata.Artist?.value);
            const license = plainText(metadata.LicenseShortName?.value);
            const url = info?.thumburl || info?.url;
            if (!/^https:\/\//.test(url || '') || !artist || !license) {
                throw new Error('Commons image URL, artist or licence unavailable');
            }
            return {
                ...override, url, artist, license,
                license_url: /^https:\/\//.test(metadata.LicenseUrl?.value || '')
                    ? metadata.LicenseUrl.value : '',
                usage_terms: plainText(metadata.UsageTerms?.value) || license
            };
        } catch (error) {
            console.warn('Museum Commons override unavailable:', title, error);
            museumCommonsOverrideRequests.delete(title);
            return null;
        } finally {
            clearTimeout(timeout);
        }
    })();
    museumCommonsOverrideRequests.set(title, request);
    return request;
}

async function getCachedDinoMedia(name) {
    const cache = getMuseumImageCache();
    const overrideCatalog = await loadMuseumOverrideCatalog();
    const configuredOverride = overrideCatalog[name];
    const override = configuredOverride?.resolve_commons_metadata
        ? await resolveMuseumCommonsOverride(configuredOverride)
        : configuredOverride;

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

    // Older versions stored exact TotalDino URLs as plain strings.
    if (typeof cached === 'string') {
        return { url: cached, source: 'totaldino' };
    }
    if (cached?.source === 'totaldino' && cached.url) return cached;

    // Refresh cached fallback art against the exact TotalDino title before reusing it.
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

    // Keep a usable cached fallback if Wikimedia is unavailable or has no exact title.
    if (cached?.url) return cached;

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

    const fallback = 'dinosaur-footprint-1-svgrepo-com.svg';
    image.addEventListener('load', () => image.classList.add('loaded'));
    image.addEventListener('error', () => {
        if (image.getAttribute('src') === fallback) image.classList.add('loaded');
        else image.src = fallback;
    });
    if (media?.url) image.src = media.url;
    else image.classList.add('loaded');
    card.dataset.museumMediaState = 'loaded';

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
            ? `<a href="${escapeChallengeHtml(media.license_url)}" target="_blank" rel="noopener">${escapeChallengeHtml(media.license)}</a>`
            : escapeChallengeHtml(media.license);
        const sourceName = media.source === 'dinopedia' ? 'Dinopedia' : 'Wikimedia Commons';
        const contributor = escapeChallengeHtml(
            media.artist || t('media.contributor', { source: sourceName })
        );
        const relatedTaxonNote = media.related_taxon && media.depicted_taxon
            ? `<span class="museum-entry-media-note">${escapeChallengeHtml(t(
                'media.relatedTaxonNotice', { depicted: media.depicted_taxon, subject: name }
            ))}</span>`
            : '';
        const editorialNote = currentLocale === 'en' && media.editorial_note
            ? `<span class="museum-entry-media-note">${escapeChallengeHtml(media.editorial_note)}</span>`
            : '';
        return `
            ${t('media.imageBy', { contributor })} · ${license}
            · <a href="${media.file_page}" target="_blank" rel="noopener">${sourceName}</a>
            ${relatedTaxonNote}
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
    if (!overlay) return false;
    if (animate && overlay) {
        if (!overlay.museumClosing) {
            overlay.classList.add('is-closing');
            const duration = window.matchMedia('(prefers-reduced-motion: reduce)').matches ? 0 : 180;
            overlay.museumClosing = new Promise(resolve => setTimeout(resolve, duration));
        }
        await overlay.museumClosing;
        if (document.getElementById('museum-entry-overlay') !== overlay) return false;
    }

    const viewer = document.getElementById('museum-image-viewer');
    if (viewer?.dismissAppOverlay) viewer.dismissAppOverlay({ restoreFocus: false });
    else viewer?.remove();
    overlay?.remove();
    document.body.style.overflow = overlay?.museumReturnState?.overflow || '';
    activeMuseumEntryMedia = null;

    if (museumEntryEscapeHandler) {
        document.removeEventListener('keydown', museumEntryEscapeHandler, true);
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

function trapMuseumOverlayTab(overlay, event) {
    if (event.key !== 'Tab') return;
    const controls = Array.from(overlay.querySelectorAll(
        'button:not([disabled]):not([hidden]), a[href]:not([hidden]), [tabindex="0"]:not([hidden])'
    ));
    const first = controls[0];
    const last = controls.at(-1);
    if (!first) {
        event.preventDefault();
        overlay.focus({ preventScroll: true });
    } else if (!controls.includes(document.activeElement)
        || (event.shiftKey && document.activeElement === first)
        || (!event.shiftKey && document.activeElement === last)) {
        event.preventDefault();
        (event.shiftKey ? last : first).focus({ preventScroll: true });
    }
}

function replaceMuseumEntryContent(overlay, html) {
    const hadFocus = overlay.contains(document.activeElement);
    overlay.querySelector('.museum-entry-dialog').innerHTML = html;
    if (hadFocus && isTopAppOverlay(overlay)) {
        overlay.querySelector('.museum-entry-close').focus({ preventScroll: true });
    }
}

function openMuseumImageViewer() {
    if (!activeMuseumEntryMedia?.url) return;

    const entry = document.getElementById('museum-entry-overlay');
    if (!entry || entry.museumDismissRequested || entry.museumClosing) return;
    const previousViewer = document.getElementById('museum-image-viewer');
    previousViewer?.dismissAppOverlay({ restoreFocus: false });
    const previouslyFocused = previousViewer?.museumPreviouslyFocused || document.activeElement;
    const viewer = document.createElement('div');
    viewer.id = 'museum-image-viewer';
    viewer.className = 'museum-image-viewer';
    viewer.setAttribute('role', 'dialog');
    viewer.setAttribute('aria-modal', 'true');
    viewer.setAttribute('aria-label', t('media.imageViewer', { name: activeMuseumEntryMedia.name }));
    viewer.tabIndex = -1;
    viewer.museumPreviouslyFocused = previouslyFocused;
    viewer.innerHTML = `
        <button class="museum-image-viewer-close" type="button" aria-label="${t('museum.closeImage')}">×</button>
        <img src="${escapeChallengeHtml(activeMuseumEntryMedia.url)}" alt="${escapeChallengeHtml(activeMuseumEntryMedia.name)}">
        <div class="museum-image-viewer-caption">
            <em>${escapeChallengeHtml(activeMuseumEntryMedia.name)}</em>
            <div class="museum-image-viewer-taxonomy">${escapeChallengeHtml(activeMuseumEntryMedia.family || '')}</div>
            <div class="museum-image-viewer-discovery">
                <span>${escapeChallengeHtml(activeMuseumEntryMedia.firstDiscovery || '')}</span>
                ${activeMuseumEntryMedia.discoveryDetails ? `<span>${escapeChallengeHtml(activeMuseumEntryMedia.discoveryDetails)}</span>` : ''}
            </div>
            <div class="museum-image-viewer-credit">${activeMuseumEntryMedia.credit}</div>
        </div>
    `;

    const closeViewer = ({ restoreFocus = true } = {}) => {
        if (!viewer.isConnected) return;
        const wasTop = isTopAppOverlay(viewer);
        document.removeEventListener('keydown', keyHandler, true);
        viewer.remove();
        if (restoreFocus && wasTop && entry.isConnected && !entry.museumDismissRequested
            && !entry.museumClosing && previouslyFocused?.isConnected
            && entry.contains(previouslyFocused)) {
            previouslyFocused.focus({ preventScroll: true });
        }
    };
    const keyHandler = event => {
        if (!isTopAppOverlay(viewer)) return;
        if (event.key === 'Escape') {
            event.preventDefault();
            event.stopImmediatePropagation();
            closeViewer();
        } else trapMuseumOverlayTab(viewer, event);
    };
    viewer.dismissAppOverlay = closeViewer;
    viewer.addEventListener('click', event => {
        if (event.target === viewer || event.target.closest('.museum-image-viewer-close')) {
            closeViewer();
        }
    });

    document.body.appendChild(viewer);
    document.addEventListener('keydown', keyHandler, true);
    viewer.querySelector('.museum-image-viewer-close').focus({ preventScroll: true });
}

// The report arrives with the genus, lineage, language and release already filled in.
function buildMuseumReportMailto(name, lineage) {
    const body = [
        t('museum.reportPrompt'),
        '',
        '',
        '---',
        `Genus: ${name}`,
        `Lineage: ${(Array.isArray(lineage) ? lineage : []).join(' › ')}`,
        `Language: ${currentLocale} · Release: ${PHYLOSAUR_RELEASE_VERSION}`
    ].join('\n');
    return `mailto:rodrigo03.villa@gmail.com?subject=${encodeURIComponent(`Phylosaur · ${name}`)}&body=${encodeURIComponent(body)}`;
}

// ── Browsing the Museum from one genus to the next ───────────────────────────
// The order is the one on screen: the discovered cards the current search and filters leave
// visible, in grid order. Opened from the result of a game there is no grid, so no browsing.
function getMuseumEntryNeighbors(name) {
    const grid = document.getElementById('museum-grid');
    if (!grid) return { names: [], index: -1 };
    const names = [...grid.querySelectorAll('.museum-card.unlocked[data-museum-entry]')]
        .filter(card => !card.hidden)
        .map(card => card.dataset.museumEntry);
    return { names, index: names.indexOf(name) };
}

function renderMuseumEntryNav(overlay, name) {
    const { names, index } = getMuseumEntryNeighbors(name);
    if (names.length < 2 || index < 0) return;
    const chevron = path => `<svg viewBox="0 0 24 24" aria-hidden="true" focusable="false"><path d="${path}"/></svg>`;
    const nav = document.createElement('div');
    nav.className = 'museum-entry-nav';
    nav.innerHTML = `
        <button type="button" class="museum-entry-nav-btn" onclick="stepMuseumEntry(-1)"
                aria-label="${escapeChallengeHtml(t('museum.prevEntry'))}" ${index === 0 ? 'disabled' : ''}>${chevron('M14.5 5.5 8 12l6.5 6.5')}</button>
        <span class="museum-entry-position" aria-live="polite">${escapeChallengeHtml(t('museum.entryPosition', { position: index + 1, total: names.length }))}</span>
        <button type="button" class="museum-entry-nav-btn" onclick="stepMuseumEntry(1)"
                aria-label="${escapeChallengeHtml(t('museum.nextEntry'))}" ${index === names.length - 1 ? 'disabled' : ''}>${chevron('M9.5 5.5 16 12l-6.5 6.5')}</button>
    `;
    overlay.classList.add('has-nav');
    overlay.append(nav);
}

// Opens the neighbouring genus. The page that is leaving is copied and turned like a page of a
// book (forward) or the arriving page is turned back over it (backward); without motion the
// content is simply replaced.
function stepMuseumEntry(direction) {
    const overlay = document.getElementById('museum-entry-overlay');
    if (!overlay || overlay.museumDismissRequested || overlay.museumClosing || overlay.museumTurning) return;
    const { names, index } = getMuseumEntryNeighbors(overlay.museumEntryName);
    const target = index < 0 ? null : names[index + direction];
    if (!target) return;

    let ghost = null;
    const dialog = overlay.querySelector('.museum-entry-dialog');
    if (dialog && !window.matchMedia('(prefers-reduced-motion: reduce)').matches) {
        const box = dialog.getBoundingClientRect();
        ghost = dialog.cloneNode(true);
        ghost.removeAttribute('id');
        ghost.setAttribute('aria-hidden', 'true');
        ghost.inert = true;
        ghost.classList.add('museum-entry-page-ghost');
        ghost.style.cssText = `left:${box.left}px;top:${box.top}px;width:${box.width}px;height:${box.height}px;`;
        ghost.dataset.scrollTop = String(dialog.scrollTop);
    }
    return showMuseumEntry(target, { step: direction, ghost });
}

function playMuseumPageTurn(overlay, direction, ghost) {
    const dialog = overlay.querySelector('.museum-entry-dialog');
    if (!dialog || !ghost) return;
    overlay.classList.add('is-turning');
    overlay.museumTurning = true;
    let finished = false;
    const finish = () => {
        if (finished) return;
        finished = true;
        ghost.remove();
        dialog.classList.remove('turn-in');
        overlay.classList.remove('is-turning');
        overlay.museumTurning = false;
    };
    // The copy always goes after the real page in the document, so that lookups inside the overlay
    // (which fill in the new page) find the real one first; stacking is decided by z-index.
    overlay.append(ghost);
    if (direction > 0) {
        ghost.classList.add('turn-away');
        ghost.addEventListener('animationend', event => {
            if (event.target === ghost) finish();
        });
    } else {
        ghost.classList.add('is-underneath');
        dialog.classList.add('turn-in');
        dialog.addEventListener('animationend', event => {
            if (event.target === dialog) finish();
        });
    }
    ghost.scrollTop = Number(ghost.dataset.scrollTop || 0);
    setTimeout(finish, 900);
}

// Horizontal swipes turn the page; vertical ones scroll the entry. Swipes that begin at the
// screen edge are left to the system (the iPhone's back gesture), and so are scrollable strips.
function attachMuseumEntrySwipe(overlay) {
    let start = null;
    overlay.addEventListener('touchstart', event => {
        const touch = event.touches[0];
        const insideScroller = (() => {
            for (let node = event.target; node && node !== overlay; node = node.parentElement) {
                if (node.scrollWidth > node.clientWidth + 2
                    && /(auto|scroll)/.test(getComputedStyle(node).overflowX)) return true;
            }
            return false;
        })();
        const atEdge = touch.clientX < 24 || touch.clientX > window.innerWidth - 24;
        start = event.touches.length === 1 && !insideScroller && !atEdge
            ? { x: touch.clientX, y: touch.clientY, time: Date.now() } : null;
    }, { passive: true });
    overlay.addEventListener('touchend', event => {
        if (!start) return;
        const touch = event.changedTouches[0];
        const dx = touch.clientX - start.x;
        const dy = touch.clientY - start.y;
        const quick = Date.now() - start.time < 800;
        start = null;
        if (quick && Math.abs(dx) > 60 && Math.abs(dx) > Math.abs(dy) * 1.6) stepMuseumEntry(dx < 0 ? 1 : -1);
    }, { passive: true });
    overlay.addEventListener('touchcancel', () => { start = null; }, { passive: true });
}

// Warms the cache for the genera on either side, so the next page arrives with its picture.
function prefetchMuseumEntryNeighbors(name) {
    const { names, index } = getMuseumEntryNeighbors(name);
    if (index < 0) return;
    setTimeout(() => {
        [names[index - 1], names[index + 1]].filter(Boolean).forEach(neighbor => {
            void getCachedDinoMedia(neighbor).then(media => {
                if (media?.url) new Image().src = media.url;
            }).catch(() => {});
            void Promise.resolve(fetchWikipediaInfo(neighbor)).catch(() => {});
        });
    }, 500);
}

async function showMuseumEntry(name, { step = 0, ghost = null } = {}) {
    // Browsing replaces the history entry: Back must leave the entry, not walk through every genus seen.
    setAppRoute(`/museum/${encodeURIComponent(name)}`, { replace: Boolean(step) });
    closeMuseumEntry();

    const dino = fullDatabase.find(item => item.nome === name);
    if (!dino) {
        // Unknown genus (stale link, inactive genus): do not leave a dead route behind.
        if (getCurrentAppRoute() === `/museum/${encodeURIComponent(name)}`) {
            setAppRoute('/museum', { replace: true });
        }
        return;
    }
    const safeName = escapeChallengeHtml(name);

    const overlay = document.createElement('div');
    overlay.id = 'museum-entry-overlay';
    overlay.className = 'museum-entry-overlay';
    overlay.setAttribute('role', 'dialog');
    overlay.setAttribute('aria-modal', 'true');
    overlay.setAttribute('aria-label', name);
    overlay.tabIndex = -1;
    overlay.museumReturnState = {
        scrollX: window.scrollX,
        scrollY: window.scrollY,
        overflow: document.body.style.overflow,
        card: Array.from(document.querySelectorAll('.museum-card.unlocked'))
            .find(card => card.dataset.museumName === name.toLowerCase()) || document.activeElement
    };
    overlay.innerHTML = `
        <article class="museum-entry-dialog">
            <button class="museum-entry-close" type="button" onclick="dismissMuseumEntry()" aria-label="${t('common.close')}">×</button>
            <div class="museum-entry-loading">${t('museum.openingName', { name: safeName })}</div>
        </article>
    `;

    overlay.addEventListener('click', event => {
        if (event.target === overlay) dismissMuseumEntry();
    });

    overlay.museumEntryName = name;
    if (step) overlay.classList.add('no-enter');
    renderMuseumEntryNav(overlay, name);
    attachMuseumEntrySwipe(overlay);
    document.body.appendChild(overlay);
    document.body.style.overflow = 'hidden';
    if (step && ghost) playMuseumPageTurn(overlay, step, ghost);
    if (typeof waitForAccountReady === 'function') {
        await waitForAccountReady();
        // Closed or replaced while waiting: do not install listeners for a dead overlay.
        if (!overlay.isConnected) return;
    }
    const ownerId = currentUserId;
    const entryRoute = `/museum/${encodeURIComponent(name)}`;
    const isCurrentEntry = () => overlay.isConnected && !overlay.museumDismissRequested
        && !overlay.museumClosing && currentUserId === ownerId
        && getCurrentAppRoute() === entryRoute
        && document.getElementById('museum-entry-overlay') === overlay;

    museumEntryEscapeHandler = event => {
        if (!isCurrentEntry() || !isTopAppOverlay(overlay)) return;
        if (event.key === 'Escape') {
            event.preventDefault();
            event.stopImmediatePropagation();
            void dismissMuseumEntry();
        } else if ((event.key === 'ArrowLeft' || event.key === 'ArrowRight')
            && !event.altKey && !event.ctrlKey && !event.metaKey && !event.shiftKey
            && !event.target.closest?.('input, textarea, select')) {
            event.preventDefault();
            stepMuseumEntry(event.key === 'ArrowRight' ? 1 : -1);
        } else trapMuseumOverlayTab(overlay, event);
    };
    document.addEventListener('keydown', museumEntryEscapeHandler, true);
    overlay.querySelector('.museum-entry-close').focus({ preventScroll: true });

    if (!Array.isArray(dino.linhagem)) {
        try {
            const discoveryRecord = museumDiscoveryRecords[name.toLowerCase()];
            const entry = await callGameApi('museum_entry', {
                name,
                museumProof: discoveryRecord?.museumProof || null,
                proofSessionIds: getStoredGameSessionIds()
            });
            if (!isCurrentEntry()) return;
            Object.assign(dino, entry.dinosaur);
        } catch (error) {
            if (!isCurrentEntry()) return;
            replaceMuseumEntryContent(overlay, `
                <button class="museum-entry-close" type="button" onclick="dismissMuseumEntry()" aria-label="${t('common.close')}">×</button>
                <div class="museum-entry-loading is-error">
                    ${t('museum.openNameError', { name: safeName })}<br>${escapeChallengeHtml(error.message)}
                </div>
            `);
            return;
        }
    }

    if (!isCurrentEntry()) return;

    const mediaPromise = getCachedDinoMedia(name);
    const wikiPromise = fetchWikipediaInfo(name);
    const paleodataPromise = loadMuseumPaleodataCatalog();
    const lineage = (dino.linhagem || [])
        .map(clade => `<span>${escapeChallengeHtml(clade)}</span>`)
        .join('<b>›</b>');
    const family = (dino.linhagem || []).at(-1) || 'Dinosauria';
    const discovery = getMuseumDiscoverySummary(
        museumDiscoveryRecords[name.toLowerCase()]
    );

    activeMuseumEntryMedia = {
        name,
        family,
        firstDiscovery: discovery.firstLabel,
        discoveryDetails: [discovery.countLabel, discovery.lastLabel].filter(Boolean).join(' · '),
        url: null,
        credit: ''
    };

    replaceMuseumEntryContent(overlay, `
            <button class="museum-entry-close" type="button" onclick="dismissMuseumEntry()" aria-label="${t('common.close')}">×</button>

        <header class="museum-entry-header">
            <h2>${safeName}</h2>
            <div class="museum-entry-meta">${escapeChallengeHtml(family)}</div>
            <div class="museum-entry-first-discovery">${escapeChallengeHtml(discovery.firstLabel)}</div>
            <button type="button" class="museum-entry-tree-link" data-cladogram-focus="${safeName}"
                    onclick="openCladogramFromMuseum(this.dataset.cladogramFocus)">${t('cladogram.viewInTree')}</button>
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

                <div class="museum-entry-footer-links">
                    <div class="museum-entry-read-more-slot"></div>
                    <a class="museum-entry-report-link" href="${escapeChallengeHtml(buildMuseumReportMailto(name, dino.linhagem))}">${t('museum.reportError')}</a>
                </div>
            </section>
        </div>
    `);

    void mediaPromise.then(media => {
        if (!isCurrentEntry()) return;
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
        caption.textContent = hasImage ? t('museum.clickEnlarge') : t('museum.noIllustration');
        activeMuseumEntryMedia = {
            ...activeMuseumEntryMedia,
            name,
            family,
            firstDiscovery: discovery.firstLabel,
            discoveryDetails: [discovery.countLabel, discovery.lastLabel].filter(Boolean).join(' · '),
            url: media?.url || null,
            credit
        };
    }).catch(error => {
        if (!isCurrentEntry()) return;
        console.warn(`Museum illustration unavailable for ${name}:`, error);
        const caption = overlay.querySelector('.museum-entry-figure figcaption');
        if (caption) caption.textContent = t('museum.noIllustration');
    });

    void wikiPromise.then(wikiInfo => {
        if (!isCurrentEntry()) return;
        const description = overlay.querySelector('.museum-entry-description');
        if (description) {
            description.textContent = wikiInfo?.description
                || t('museum.noSummary');
        }

        const readMoreSlot = overlay.querySelector('.museum-entry-read-more-slot');
        if (readMoreSlot && wikiInfo?.url) {
            readMoreSlot.innerHTML = `
                ${wikiInfo.isLanguageFallback
                    ? `<p class="encyclopedia-language-note">${t('encyclopedia.englishFallback')}</p>` : ''}
                <a class="museum-entry-read-more" href="${escapeChallengeHtml(wikiInfo.url)}"
                   target="_blank" rel="noopener">
                    <span>${t('museum.readWikipedia')}</span>
                    <i class="ui-icon ui-icon-external" aria-hidden="true"></i>
                </a>
            `;
        }
    }).catch(error => {
        if (!isCurrentEntry()) return;
        console.warn(`Museum summary unavailable for ${name}:`, error);
        const description = overlay.querySelector('.museum-entry-description');
        if (description) description.textContent = t('museum.noSummary');
    });

    prefetchMuseumEntryNeighbors(name);

    void paleodataPromise.then(paleodataCatalog => {
        if (!isCurrentEntry()) return;
        const slot = overlay.querySelector('.museum-entry-paleodata-slot');
        if (!slot) return;
        const paleodata = paleodataCatalog.taxa[name] || null;
        const html = renderMuseumPaleodata(paleodata, paleodataCatalog.timeline);
        if (html) slot.innerHTML = html;
        else slot.remove();
    }).catch(error => {
        if (!isCurrentEntry()) return;
        console.warn(`Museum fossil data unavailable for ${name}:`, error);
        overlay.querySelector('.museum-entry-paleodata-slot')?.remove();
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

function renderMuseumCladeFilters(unlockedSet) {
    const collections = MUSEUM_ATLAS_COLLECTIONS.map(definition =>
        getMuseumAtlasCollection(definition, unlockedSet)
    );

    return `
        <label class="museum-toolbar-filter" for="museum-clade-select">
            <span>${t("museum.cladeFilter")}</span>
            <select id="museum-clade-select" aria-label="${t("museum.filterByClade")}"
                    onchange="openMuseumClade(this.value)">
                <option value="all" ${selectedMuseumClade === "all" ? "selected" : ""}>${t("museum.allClades")}</option>
                <option value="other" ${selectedMuseumClade === "other" ? "selected" : ""}>${t("museum.otherClades")}</option>
                ${collections.map(collection => `
                    <option value="${escapeChallengeHtml(collection.clade)}" ${selectedMuseumClade === collection.clade ? "selected" : ""}>
                        ${escapeChallengeHtml(collection.title)}
                    </option>
                `).join("")}
            </select>
        </label>
    `;
}

function renderMuseumLevelFilter() {
    const levels = [
        ["all", t("museum.all")],
        ["muito_facil", t("level.name1")],
        ["facil", t("level.name2")],
        ["normal", t("level.name3")],
        ["dificil", t("level.name4")],
        ["muito_dificil", t("level.name5")]
    ];

    return `
        <label class="museum-toolbar-filter" for="museum-level-select">
            <span>${t("museum.difficultyFilter")}</span>
            <select id="museum-level-select" aria-label="${t("museum.filterLevel")}"
                    onchange="switchMuseumLevel(this.value)">
                ${levels.map(([value, label]) => `
                    <option value="${value}" ${selectedMuseumLevel === value ? "selected" : ""}>${label}</option>
                `).join("")}
            </select>
        </label>
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
            <div class="museum-card-name" title="${safeName}">${safeName}</div>
            <div class="museum-card-clade">${escapeChallengeHtml(lastClade)}</div>
            <div class="museum-card-discovery">
                <span>${escapeChallengeHtml(discovery.firstLabel)}</span>
            </div>
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
                <span id="museum-empty-text">${t('museum.noMatch')}</span>
                <button type="button" class="btn-hint museum-clear-filters" onclick="clearMuseumFilters()">${t('museum.clearFilters')}</button>
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
    grid.innerHTML = renderAppState(t('museum.preparingGenera'), { compact: true });
    museumSpecimenRenderFrame = requestAnimationFrame(() => {
        museumSpecimenRenderFrame = null;
        if (!grid.isConnected) return;
        renderMuseumSpecimensNow();
    });
    return false;
}

function initializeMuseumGallery() {
    const specimensPanel = document.getElementById("museum-specimens-panel");
    if (specimensPanel) specimensPanel.hidden = false;
    const unlockedToggle = document.getElementById("museum-unlocked-only");
    if (unlockedToggle) unlockedToggle.checked = museumUnlockedOnly;
    const searchInput = document.getElementById("museum-search-input");
    if (searchInput && museumSearchText.trim().toLowerCase() === museumSearchQuery) {
        searchInput.value = museumSearchText;
    }

    if (ensureMuseumSpecimensRendered()) {
        applyMuseumFilters();
        initializeMuseumCardMediaLoading();
    }
}

function updateMuseumCladeFilterDisplay() {
    const selector = document.getElementById("museum-clade-select");
    if (selector) selector.value = selectedMuseumClade;
}

function openMuseumClade(clade) {
    const isKnownClade = MUSEUM_ATLAS_COLLECTIONS.some(definition => definition.clade === clade);
    selectedMuseumClade = clade === "all" || clade === "other" || isKnownClade ? clade : "all";
    updateMuseumCladeFilterDisplay();
    applyMuseumFilters();
}

function museumLineageMatchesClade(lineage, clade) {
    if (clade === "all") return true;

    const encodedLineage = `|${lineage || ""}|`;
    if (clade === "other") {
        return !MUSEUM_ATLAS_COLLECTIONS.some(definition =>
            encodedLineage.includes(`|${definition.clade}|`)
        );
    }

    return encodedLineage.includes(`|${clade}|`);
}

function clearMuseumCladeFilter() {
    openMuseumClade("all");
}

async function showMuseum() {
    setAppRoute('/museum');
    setHeaderControls('museum');
    const appContent = document.getElementById('app-content');
    
    appContent.innerHTML = `<div class="game-card">${renderAppState(t('museum.loading'))}</div>`;
    const loadingCard = appContent.firstElementChild;
    if (typeof waitForAccountReady === 'function') {
        await waitForAccountReady();
        if (appContent.firstElementChild !== loadingCard) return;
    }
    const museumOwnerId = currentUserId;
    const isCurrentRequest = () => appContent.firstElementChild === loadingCard
        && currentUserId === museumOwnerId;
    // Guest -> account while loading (the startup lookup finished after the wait
    // cap): load again for the account instead of leaving the loading card up.
    // A switch between two accounts still discards the old result.
    const ownerChanged = () => appContent.firstElementChild === loadingCard
        && museumOwnerId === null && Boolean(currentUserId);

    try {
        let museumDatabase = fullDatabase;
        if (!fullDatabase || fullDatabase.length === 0) {
            const catalog = await callGameApi('catalog');
            if (ownerChanged()) return showMuseum();
            if (!isCurrentRequest()) return;
            museumDatabase = catalog.dinosaurs || [];
        }
        const [catalogDatabase, discoveryRecords] = await Promise.all([
            ensureMuseumCatalogLineages(museumDatabase),
            getDiscoveryRecords()
        ]);
        if (ownerChanged()) return showMuseum();
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
                    <button type="button" class="btn-hint cladogram-open-button" onclick="showCladogram({ focus: '' })">
                        ${t('cladogram.openTree')}
                    </button>
                </div>

                <div id="museum-specimens-panel">
                <div class="museum-toolbar">
                    <label class="museum-search" for="museum-search-input">
                        <span>${t('museum.search')}</span>
                        <input id="museum-search-input" type="search"
                               value="${escapeChallengeHtml(museumSearchQuery)}"
                               placeholder="${t('museum.searchPlaceholder')}"
                               autocomplete="off"
                               oninput="updateMuseumSearch(this.value)">
                    </label>

                    ${renderMuseumCladeFilters(unlockedSet)}

                    ${renderMuseumLevelFilter()}
                </div>

                <div class="museum-filter-row">
                    <div class="museum-filter-summary" id="museum-filter-summary" aria-live="polite">
                        ${t('museum.showing', {
                            count: totalCount,
                            genera: t('museum.genusMany'),
                            clade: '',
                            unlocked: totalUnlocked
                        })}
                    </div>
                    <label class="museum-unlocked-toggle" for="museum-unlocked-only">
                        <input type="checkbox" id="museum-unlocked-only" onchange="applyMuseumFilters()">
                        <span>${t('museum.unlockedOnly')}</span>
                    </label>
                </div>

                <div class="museum-grid" id="museum-grid" data-render-state="pending">
                </div>
                </div>
            </div>
            <div id="clade-info"></div>`;
        appContent.innerHTML = html;

        updateMuseumCladeFilterDisplay();
        initializeMuseumGallery();
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
    const validLevels = ['all', 'muito_facil', 'facil', 'normal', 'dificil', 'muito_dificil'];
    selectedMuseumLevel = validLevels.includes(level) ? level : 'all';
    const selector = document.getElementById('museum-level-select');
    if (selector) selector.value = selectedMuseumLevel;
    applyMuseumFilters();
}

function updateMuseumSearch(value) {
    museumSearchText = String(value || '');
    museumSearchQuery = museumSearchText.trim().toLowerCase();
    if (museumFilterFrame !== null) return;
    museumFilterFrame = requestAnimationFrame(() => {
        museumFilterFrame = null;
        applyMuseumFilters();
    });
}

function applyMuseumFilters() {
    const unlockedToggle = document.getElementById('museum-unlocked-only');
    if (unlockedToggle) museumUnlockedOnly = unlockedToggle.checked;
    saveMuseumFilters();

    const cards = museumFilterCards.length
        ? museumFilterCards
        : [...document.querySelectorAll('.museum-card[data-museum-level]')];
    if (cards.length === 0) return;

    let visibleCount = 0;
    let visibleUnlocked = 0;
    const unlockedOnly = museumUnlockedOnly;

    cards.forEach(card => {
        const matchesLevel = (selectedMuseumLevel === 'all'
            || card.dataset.museumLevel === selectedMuseumLevel)
            && (!unlockedOnly || card.dataset.museumUnlocked === 'true');
        const matchesSearch = !museumSearchQuery
            || (card.dataset.museumUnlocked === 'true' && card.dataset.museumName.includes(museumSearchQuery));
        const matchesClade = museumLineageMatchesClade(
            card.dataset.museumLineage, selectedMuseumClade
        );
        const isVisible = matchesLevel && matchesSearch && matchesClade;

        card.hidden = !isVisible;
        if (!isVisible) return;

        visibleCount += 1;
        if (card.dataset.museumUnlocked === 'true') visibleUnlocked += 1;
    });

    const summary = document.getElementById('museum-filter-summary');
    if (summary) {
        const generaLabel = visibleCount === 1
            ? t('museum.genusOne')
            : t('museum.genusMany');
        const cladeLabel = selectedMuseumClade === 'all'
            ? ''
            : selectedMuseumClade === 'other'
                ? t('museum.outsideMainClades')
                : t('museum.inClade', { clade: selectedMuseumClade });
        summary.textContent = t('museum.showing', {
            count: visibleCount,
            genera: generaLabel,
            clade: cladeLabel,
            unlocked: visibleUnlocked
        });
    }

    const emptyState = document.getElementById('museum-empty-state');
    if (emptyState) {
        emptyState.hidden = visibleCount !== 0;
        const emptyText = selectedMuseumClade === 'all'
            ? t('museum.noMatch')
            : selectedMuseumClade === 'other'
                ? t('museum.noOtherCladeMatch')
                : t('museum.noCladeMatch', { clade: selectedMuseumClade });
        // The message lives in its own span so the "clear filters" button survives updates.
        const emptyTextNode = document.getElementById('museum-empty-text');
        if (emptyTextNode) emptyTextNode.textContent = emptyText;
        else emptyState.textContent = emptyText;
    }
}

// Filters persist across visits, so a returning player can land on "no match":
// one click returns to the whole Museum.
function clearMuseumFilters() {
    selectedMuseumClade = 'all';
    selectedMuseumLevel = 'all';
    museumSearchText = '';
    museumSearchQuery = '';
    museumUnlockedOnly = false;
    const searchInput = document.getElementById('museum-search-input');
    if (searchInput) searchInput.value = '';
    const levelSelect = document.getElementById('museum-level-select');
    if (levelSelect) levelSelect.value = 'all';
    const unlockedToggle = document.getElementById('museum-unlocked-only');
    if (unlockedToggle) unlockedToggle.checked = false;
    updateMuseumCladeFilterDisplay();
    applyMuseumFilters();
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
    const byDay = Array.isArray(data.byDay) ? data.byDay : [];
    const maxStarted = Math.max(1, ...byDay.map(day => Number(day.started || 0)));
    const chart = byDay.map(day => {
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

    // Visitors per browser language; languages without a translation are flagged.
    let languageNames = null;
    try { languageNames = new Intl.DisplayNames([currentLocale], { type: 'language' }); } catch (_error) {}
    const languageRows = (Array.isArray(data.byLanguage) ? data.byLanguage : []).map(entry => {
        const name = entry.language === 'unknown'
            ? t('analytics.languageUnknown')
            : (languageNames?.of(entry.language) || entry.language);
        const flag = entry.translated === false
            ? `<span class="analytics-language-missing">${t('analytics.languageMissing')}</span>`
            : '<span></span>';
        return `<div class="analytics-breakdown-row"><span>${escapeChallengeHtml(name)}</span><strong>${Number(entry.visitors) || 0}</strong>${flag}</div>`;
    }).join('');

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
            <span>${t('analytics.playerMuseum', {
                count: player.museumUnlocked || 0, total: summary.museumTotal || '?'
            })}</span>
            <time>${t('analytics.lastDaily', { date: escapeChallengeHtml(lastPlayed) })}</time>
        </div>`;
    }).join('');

    const pagination = data.pagination || {};
    const hasTruncatedData = Object.values(pagination).some(Boolean);

    // Totals since the first recorded game; shown only once the server sends them.
    const allTime = summary.allTime;
    const allTimeSince = allTime?.firstSessionAt
        ? t('analytics.allTimeSince', {
            date: new Date(allTime.firstSessionAt).toLocaleDateString(
                currentLocale === 'pt-BR' ? 'pt-BR' : currentLocale
            )
        })
        : '';
    // How well the period's new visitors come back, and how many become account holders.
    const retention = summary.retention;
    const percentOf = (part, whole) => whole > 0 ? `${Math.round((part / whole) * 100)}%` : '–';
    const retentionSection = retention ? `
        <section class="analytics-section analytics-retention">
            <h3>${t('analytics.retentionTitle')}</h3>
            <p class="analytics-section-note">${t('analytics.retentionCopy')}</p>
            <div class="analytics-metrics">
                ${analyticsMetricCard(t('analytics.retentionNextDay'), percentOf(retention.nextDay.returned, retention.nextDay.eligible), t('analytics.retentionSample', { count: retention.nextDay.eligible }))}
                ${analyticsMetricCard(t('analytics.retentionWeek'), percentOf(retention.week.returned, retention.week.eligible), t('analytics.retentionSample', { count: retention.week.eligible }))}
                ${analyticsMetricCard(t('analytics.retentionRegulars'), retention.regularPlayers, t('analytics.retentionRegularsDetail'))}
                ${analyticsMetricCard(t('analytics.retentionRegularsAccount'), percentOf(retention.regularPlayersWithAccount, retention.regularPlayers), t('analytics.retentionOutOf', { count: retention.regularPlayersWithAccount, total: retention.regularPlayers }))}
                ${analyticsMetricCard(t('analytics.retentionAccountVisitors'), percentOf(retention.visitorsWithAccount, retention.totalVisitors), t('analytics.retentionOutOf', { count: retention.visitorsWithAccount, total: retention.totalVisitors }))}
            </div>
        </section>` : '';

    const allTimeSection = `
        <section class="analytics-section analytics-all-time">
            <h3>${t('analytics.allTimeTitle')}</h3>
            ${allTime ? `
            <p class="analytics-section-note">${t('analytics.allTimeCopy')}</p>
            <div class="analytics-metrics">
                ${analyticsMetricCard(t('analytics.uniqueVisitors'), allTime.uniqueVisitors, allTimeSince)}
                ${analyticsMetricCard(t('analytics.gamesStarted'), allTime.totalSessions)}
                ${analyticsMetricCard(t('analytics.gamesCompleted'), allTime.completedGames)}
                ${analyticsMetricCard(t('analytics.wins'), allTime.wins)}
                ${analyticsMetricCard(t('analytics.registeredAccounts'), summary.registeredAccounts)}
            </div>` : `
            <p class="analytics-section-note">${t('analytics.allTimeUnavailable')}</p>`}
        </section>`;

    appContent.innerHTML = `
    <div class="game-card analytics-dashboard">
        <div class="analytics-header">
            <div>
                <div class="friends-kicker">${t('analytics.private')}</div>
                <h2>${t('analytics.title')}</h2>
                <p>${t('analytics.privacyCopy')}</p>
            </div>
            <div class="analytics-range" role="group" aria-label="${t('analytics.period')}">
                ${[7, 30, 90].map(period => `<button class="btn-hint btn-header ${period === data.days ? 'active' : ''}" aria-pressed="${period === data.days}" onclick="showAnalyticsDashboard(${period})">${t('analytics.days', { count: period })}</button>`).join('')}
            </div>
        </div>

        ${allTimeSection}

        <section class="analytics-section">
        <h3>${t('analytics.days', { count: data.days })}</h3>
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
        </section>

        ${retentionSection}

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

        <section class="analytics-section analytics-languages">
            <h3>${t('analytics.byLanguage')}</h3>
            <p class="analytics-section-note">${t('analytics.languageCopy')}</p>
            <div class="analytics-breakdown">${languageRows || `<p class="empty-state">${t('analytics.noGames')}</p>`}</div>
        </section>

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