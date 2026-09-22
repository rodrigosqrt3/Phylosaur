// ═══════════════════════════════════════════════
// SCREENS AND INTERFACE LOGIC
// ═══════════════════════════════════════════════
function escapeChallengeHtml(value) {
    return String(value ?? '').replace(/[&<>'"]/g, character => ({
        '&': '&amp;', '<': '&lt;', '>': '&gt;', "'": '&#39;', '"': '&quot;'
    })[character]);
}

async function showDifficultySelection() {
    setAppRoute('/');
    if (typeof stopChallengeStatusPolling === 'function') stopChallengeStatusPolling();
    const completionStatus = getImmediateDailyCompletionStatus();
    setHeaderControls('difficulty');
    const appContent = document.getElementById('app-content');
    
    appContent.innerHTML = `            
        <div class="game-card difficulty-home">
        <h2 class="difficulty-home-title screen-title">Daily Challenge</h2>
        <p class="difficulty-home-date">
            ${getCurrentDateFormatted()} - Choose a level
        </p>
        <p class="difficulty-home-countdown">
        Next daily challenge in <span id="countdown-timer">--:--:--</span>
        </p>

        <div class="difficulty-home-levels">
            ${generateDifficultyButton('muito_facil', 'LEVEL I', 'I', '', completionStatus.muito_facil)}
            ${generateDifficultyButton('facil', 'LEVEL II', 'II', '', completionStatus.facil)}
            ${generateDifficultyButton('normal', 'LEVEL III', 'III', '', completionStatus.normal)}
            ${generateDifficultyButton('dificil', 'LEVEL IV', 'IV', '', completionStatus.dificil)}
            ${generateDifficultyButton('muito_dificil', 'LEVEL V', 'V', '', completionStatus.muito_dificil)}
        </div>

        <div class="difficulty-home-actions-wrap">
            <div class="difficulty-home-actions">
            <button class="btn-hint btn-large btn-menu-action" onclick="showHowToPlay()">
                How to Play
            </button>
            <button class="btn-hint btn-large btn-menu-action" onclick="showPracticeMode()">
                Practice Mode
            </button>
            <button class="btn-hint btn-friends btn-large btn-menu-action" onclick="showFriendChallenges()">
                Play with Friends
            </button>
            </div>
        </div>
    `;
    startCountdown();
    void refreshDifficultySelectionAccountState();
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
            mark.setAttribute('aria-label', 'Completed');
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
            <div class="friends-kicker">Private Challenge</div>
            <h2 class="screen-title">Play with Friends</h2>
            <p>Create a private challenge or enter a six-character code. Everyone receives the same hidden dinosaur and plays on their own tree.</p>
        </div>

        <div class="friends-grid">
            <section class="friend-panel">
                <h3>Create a Challenge</h3>
                <label class="friend-label" for="challenge-create-name">Your name</label>
                <input class="friend-input" id="challenge-create-name" maxlength="24" value="${escapeChallengeHtml(suggestedName)}" placeholder="Player name">

                <label class="friend-label" for="challenge-difficulty">Level</label>
                <select class="friend-input" id="challenge-difficulty">
                    <option value="muito_facil">Level I</option>
                    <option value="facil">Level II</option>
                    <option value="normal" selected>Level III</option>
                    <option value="dificil">Level IV</option>
                    <option value="muito_dificil">Level V</option>
                </select>

                <button class="btn-guess friend-action" id="create-challenge-btn" onclick="createFriendChallenge()">Create Code</button>
            </section>

            <div class="friends-divider" aria-hidden="true"><span>or</span></div>

            <section class="friend-panel">
                <h3>Join a Challenge</h3>
                <label class="friend-label" for="challenge-join-name">Your name</label>
                <input class="friend-input" id="challenge-join-name" maxlength="24" value="${escapeChallengeHtml(suggestedName)}" placeholder="Player name">

                <label class="friend-label" for="challenge-code">Challenge code</label>
                <input class="friend-input challenge-code-input" id="challenge-code" maxlength="6" value="${escapeChallengeHtml(code)}" placeholder="RAPTOR" autocomplete="off" autocapitalize="characters" spellcheck="false"
                       oninput="this.value=this.value.toUpperCase().replace(/[^A-Z0-9]/g,'').slice(0,6)"
                       onkeydown="if(event.key==='Enter') joinFriendChallenge()">

                <button class="btn-hint friend-action" id="join-challenge-btn" onclick="joinFriendChallenge()">Enter Challenge</button>
            </section>
        </div>

        <p class="friends-note">Codes expire after seven days. Friend challenges can unlock Museum entries, but do not affect Daily streaks or statistics.</p>
    </div>`;

    if (code) document.getElementById('challenge-join-name')?.focus();
}

async function createFriendChallenge() {
    const nameInput = document.getElementById('challenge-create-name');
    const difficultyInput = document.getElementById('challenge-difficulty');
    const button = document.getElementById('create-challenge-btn');
    const playerName = nameInput?.value.trim().slice(0, 24) || 'Player';

    button.disabled = true;
    button.textContent = 'Creating…';
    try {
        const data = await callGameApi('create_challenge', {
            difficulty: difficultyInput.value,
            playerName
        });
        localStorage.setItem(getChallengeSessionStorageKey(data.challenge.code), data.sessionId);
        await startFriendChallengeFromPayload(data);
    } catch (error) {
        await customAlert('Could Not Create Challenge', error.message);
        button.disabled = false;
        button.textContent = 'Create Code';
    }
}

async function joinFriendChallenge() {
    const nameInput = document.getElementById('challenge-join-name');
    const codeInput = document.getElementById('challenge-code');
    const button = document.getElementById('join-challenge-btn');
    const playerName = nameInput?.value.trim().slice(0, 24) || 'Player';
    const code = codeInput?.value.toUpperCase().replace(/[^A-Z0-9]/g, '') || '';

    if (code.length !== 6) {
        await customAlert('Invalid Code', 'Enter the complete six-character challenge code.');
        codeInput?.focus();
        return;
    }

    button.disabled = true;
    button.textContent = 'Entering…';
    try {
        await loadChallengeDatabase(code, playerName);
    } catch (error) {
        await customAlert('Could Not Enter Challenge', error.message);
        button.disabled = false;
        button.textContent = 'Enter Challenge';
    }
}

function showPracticeMode() {
    setAppRoute('/practice');
    setHeaderControls('practice-menu');
    const appContent = document.getElementById('app-content');
    
    appContent.innerHTML = `
    <div class="game-card difficulty-home practice-menu">
        <h2 class="difficulty-home-title screen-title">Practice Mode</h2>
        <p class="difficulty-home-date">
        Choose a level and play as often as you like
        </p>

        <div class="difficulty-home-levels">
        ${generatePracticeDifficultyButton('muito_facil', 'LEVEL I', 'I')}
        ${generatePracticeDifficultyButton('facil', 'LEVEL II', 'II')}
        ${generatePracticeDifficultyButton('normal', 'LEVEL III', 'III')}
        ${generatePracticeDifficultyButton('dificil', 'LEVEL IV', 'IV')}
        ${generatePracticeDifficultyButton('muito_dificil', 'LEVEL V', 'V')}
        </div>
    </div>
    `;
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
        ? '<span class="difficulty-completion-mark" aria-label="Completed">✓</span>'
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
    alert('Login to view statistics');
    return;
    }
    setAppRoute('/stats');
    setHeaderControls('stats');

    const [statsResult, diffStatsResult, recentGamesResult, achievementsResult, achievementHistoryResult] = await Promise.all([
        sb.from('statistics').select('*').eq('user_id', currentUserId).single(),
        sb.from('difficulty_stats').select('*').eq('user_id', currentUserId),
        sb.from('daily_results').select('*').eq('user_id', currentUserId)
            .order('created_at', { ascending: false }).limit(10),
        sb.from('achievements').select('achievement_id').eq('user_id', currentUserId),
        sb.from('daily_results')
            .select('difficulty, guess_count, hint_history, won')
            .eq('user_id', currentUserId)
            .eq('won', true)
    ]);
    const stats = statsResult.data;
    const diffStats = diffStatsResult.data;
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

    try {
        const accountSynchronization = await syncAccountAchievements();
        accountSynchronization.unlockedIds.forEach(id => unlockedAchievements.add(id));
        supplementalAchievementProgress = accountSynchronization.progress;
    } catch (error) {
        console.error('Extended achievement synchronization failed:', error);
    }

    const achievementProgress = buildAchievementProgress(
        stats,
        achievementHistory,
        supplementalAchievementProgress
    );
    const unlockedAchievementCount = ACHIEVEMENT_DEFINITIONS
        .filter(achievement => unlockedAchievements.has(achievement.id)).length;

    const appContent = document.getElementById('app-content');

    appContent.innerHTML = `


    <div class="game-card">
        <h2 class="screen-title">Statistics</h2>

        <div class="stats-player">
        Player: <span>${currentUser}</span>
        </div>

        <div class="stats stats-overview">
        <div class="stat"><div class="stat-value">${gamesPlayed}</div><div class="stat-label">Games Played</div></div>
        <div class="stat"><div class="stat-value">${gamesWon}</div><div class="stat-label">Games Won</div></div>
        <div class="stat"><div class="stat-value">${winRate}%</div><div class="stat-label">Success Rate</div></div>
        <div class="stat"><div class="stat-value">${stats?.best_score || '-'}</div><div class="stat-label">Best Score</div></div>
        </div>

        ${generateStreakDisplay(streakData)}

        <div class="stats-section">
        <h3 class="stats-section-title">Performance by Level</h3>
        ${generateDifficultyStats(diffStats)}
        </div>

        <div class="achievements-panel">
        <div class="achievements-heading">
            <h3>Achievements</h3>
            <span>${unlockedAchievementCount} / ${ACHIEVEMENT_DEFINITIONS.length} unlocked</span>
        </div>
        ${generateAchievements(unlockedAchievements, achievementProgress)}
        </div>

        <div class="stats-section">
        <h3 class="stats-section-title">Recent Games</h3>
        ${generateRecentGames(recentGames)}
        </div>
    </div>
    `;
}

function generateStreakDisplay(streakData) {
    if (!currentUser) return '';

    if (!streakData || streakData.current === 0) {
    return `
        <div class="streak-card streak-card-empty">
        <div class="streak-empty-title">No streak yet</div>
        <div class="streak-copy">Play a daily challenge to start one.</div>
        </div>
    `;
    }

    return `
    <div class="streak-card streak-card-active">
        <div class="streak-value">◆ ${streakData.current}</div>
        <div class="streak-heading">Current streak</div>
        <div class="streak-copy">Best: ${streakData.best} days</div>
        <div class="streak-meta">Last played: ${streakData.lastPlayed || 'Never'}</div>
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
    appContent.innerHTML = `<div class="game-card about-screen">${renderAppState('Loading About Phylosaur…')}</div>`;

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
                <h2 class="screen-title about-screen-title">About Phylosaur</h2>
                <div class="about-screen-body">${article.innerHTML}</div>
                <button class="btn-new-game" onclick="navigateToAppRoute('/')">Return to Levels</button>
            </div>`;
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
            'Could not load About Phylosaur.', { type: 'error', detail: error.message }
        )}<button class="btn-new-game" onclick="navigateToAppRoute('/')">Return to Levels</button></div>`;
    }
}

async function showHowToPlay() {
    const action = await showModal({
    title: 'How to Play',
    message: `
        <div class="how-to-play-content">
        <p class="how-to-play-intro">A mystery dinosaur is hidden each day. Your goal is to identify it using the clues revealed by each guess.</p>
        
        <div class="how-to-play-step">
            <strong>1. Guess a genus</strong><br>
            Type any dinosaur name and submit. The tree will reveal how closely related your guess is to the target.
        </div>

        <div class="how-to-play-step">
            <strong>2. Read the tree</strong><br>
            Each guess reveals the deepest clade shared with the mystery dinosaur. The closer on the tree, the warmer you are.
        </div>

        <div class="how-to-play-step">
            <strong>3. Hints</strong><br>
            You have 3 hints per challenge. Hints reveal the next clade, then clues about the genus name once the full lineage is known. You must make 2 guesses between hints.
        </div>

        <div class="how-to-play-step">
            <strong>4. Five difficulty levels</strong><br>
            Level I features well-known genera. Level V features obscure taxa requiring broad taxonomic knowledge.
        </div>

        <div class="how-to-play-step how-to-play-tip">
            <strong>Tip</strong><br>
            Click any node on the tree to read about that clade. New here? Start with Level I.
            <br><br>
            <span class="how-to-play-note">
            The game currently has no guess limit while I evaluate what feels most balanced. 
            This may change in future updates.
            </span>
        </div>
        </div>
    `,
    buttons: [
        { text: 'Interactive Tutorial', value: 'tutorial', primary: true },
        { text: 'Close', value: 'close', primary: false }
    ],
    closeOnOverlay: true
    });

    if (action === 'tutorial') showInteractiveTutorial();
}

const FIRST_RUN_TUTORIAL_KEY = 'phylosaur-tutorial-v1-complete';
let tutorialStepIndex = 0;
let tutorialDemoTried = false;
let tutorialKeyHandler = null;
let tutorialPreviouslyFocused = null;
let tutorialPreviousBodyOverflow = '';

const INTERACTIVE_TUTORIAL_STEPS = [
    {
        kicker: 'How Phylosaur Works',
        title: 'Find the hidden dinosaur',
        copy: 'Phylosaur is a deduction game. Every guess teaches you where the mystery genus belongs on the evolutionary tree.',
        visual: `
            <svg class="tutorial-welcome-tree" viewBox="0 0 280 150" aria-hidden="true">
                <path class="tutorial-welcome-branch" d="M140 132V98M140 98H62V62M140 98H218V62M62 62H30V28M62 62H94V28M218 62H186V28M218 62H250V28"></path>
                <circle class="tutorial-welcome-node" cx="30" cy="28" r="8"></circle>
                <circle class="tutorial-welcome-node" cx="94" cy="28" r="8"></circle>
                <circle class="tutorial-welcome-node" cx="186" cy="28" r="8"></circle>
                <circle class="tutorial-welcome-node is-target" cx="250" cy="28" r="15"></circle>
                <text class="tutorial-welcome-question" x="250" y="29">?</text>
                <circle class="tutorial-welcome-root" cx="140" cy="132" r="7"></circle>
            </svg>
            <div class="tutorial-welcome-line">
                <span>Guess</span><i class="ui-icon ui-icon-arrow-right" aria-hidden="true"></i>
                <span>Compare</span><i class="ui-icon ui-icon-arrow-right" aria-hidden="true"></i>
                <span>Follow the branches</span>
            </div>
        `
    },
    {
        kicker: 'Step 1',
        title: 'Make a guess',
        copy: 'Choose a dinosaur genus from the suggestions. Try the sample below to see the kind of clue a guess creates.',
        visual: `
            <div class="tutorial-guess-demo">
                <div class="tutorial-fake-input"><em>Triceratops</em></div>
                <button class="tutorial-demo-action" type="button">Try Sample Guess</button>
                <div class="tutorial-demo-feedback" aria-live="polite">
                    <strong>Ornithischia</strong>
                    <span>4/9 shared nodes · 44% proximity</span>
                </div>
            </div>
        `
    },
    {
        kicker: 'Step 2',
        title: 'Follow the best trail',
        copy: 'The brightest connected branch is your strongest route so far. A warmer guess reaches deeper into the target lineage.',
        visual: `
            <div class="tutorial-tree-demo" aria-label="Example evolutionary trail">
                <div class="tutorial-tree-node is-root">Dinosauria</div>
                <div class="tutorial-tree-link is-best"><i class="ui-icon ui-icon-arrow-down" aria-hidden="true"></i></div>
                <div class="tutorial-tree-node is-best">Ornithischia</div>
                <div class="tutorial-tree-split">
                    <div><span><i class="ui-icon ui-icon-arrow-down-left" aria-hidden="true"></i></span><div class="tutorial-tree-node is-guess">Triceratops</div></div>
                    <div><span class="is-best"><i class="ui-icon ui-icon-arrow-down-right" aria-hidden="true"></i></span><div class="tutorial-tree-node is-best">Best trail</div></div>
                </div>
            </div>
        `
    },
    {
        kicker: 'Step 3',
        title: 'Using hints',
        copy: 'A hint reveals the next clade in the hidden lineage. Once the full lineage is known, remaining hints reveal clues about the genus name. You have three, and must make two guesses before requesting another.',
        visual: `
            <div class="tutorial-hint-demo">
                <div class="tutorial-hint-count"><strong>3</strong><span>hints available</span></div>
                <div class="tutorial-hint-rule"><span>◇</span><span>Guess</span><span>◇</span><span>Guess</span><span>◆</span><span>Hint</span></div>
            </div>
        `
    },
    {
        kicker: 'Choose a Level',
        title: 'Begin with Level I',
        copy: 'Daily levels share the same rules but use different pools of dinosaurs. Start familiar, then work toward the obscure taxa in Level V.',
        visual: `
            <div class="tutorial-levels" aria-hidden="true">
                <span class="is-recommended">I<small>START</small></span>
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
        <span class="${index === tutorialStepIndex ? 'active' : ''}" aria-label="Step ${index + 1} of ${INTERACTIVE_TUTORIAL_STEPS.length}"></span>
    `).join('');
    overlay.querySelector('.tutorial-kicker').textContent = step.kicker;
    overlay.querySelector('.tutorial-title').textContent = step.title;
    overlay.querySelector('.tutorial-copy').textContent = step.copy;
    overlay.querySelector('.tutorial-visual').innerHTML = step.visual;

    const backButton = overlay.querySelector('.tutorial-back');
    const nextButton = overlay.querySelector('.tutorial-next');
    backButton.hidden = isFirst;
    nextButton.textContent = isLast ? 'Start Playing' : requiresDemo ? 'Try the Guess First' : 'Next';
    nextButton.disabled = requiresDemo;

    const demoButton = overlay.querySelector('.tutorial-demo-action');
    const demoFeedback = overlay.querySelector('.tutorial-demo-feedback');
    if (tutorialStepIndex === 1 && tutorialDemoTried) {
        demoButton.textContent = 'Guess Revealed';
        demoButton.disabled = true;
        demoFeedback.classList.add('visible');
    }

    demoButton?.addEventListener('click', event => {
        tutorialDemoTried = true;
        event.currentTarget.textContent = 'Guess Revealed';
        event.currentTarget.disabled = true;
        demoFeedback?.classList.add('visible');
        nextButton.disabled = false;
        nextButton.textContent = 'Next';
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
            <button class="tutorial-skip" type="button">${firstRun ? 'Skip tutorial' : 'Close'}</button>
            <div class="tutorial-progress" aria-label="Tutorial progress"></div>
            <div class="tutorial-kicker"></div>
            <h2 class="tutorial-title" id="tutorial-title"></h2>
            <p class="tutorial-copy"></p>
            <div class="tutorial-visual"></div>
            <div class="tutorial-actions">
                <button class="tutorial-back" type="button">Back</button>
                <button class="tutorial-next" type="button">Next</button>
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

function generateDifficultyStats(diffStats) {
    const diffNames = {
    'muito_facil': 'Level I',
    'facil': 'Level II',
    'normal': 'Level III',
    'dificil': 'Level IV',
    'muito_dificil': 'Level V'
    };

    if (!diffStats || diffStats.length === 0) {
    return '<p class="empty-stats">No games completed yet.</p>';
    }

    let html = '';
    diffStats.forEach(stat => {
    const winRate = stat.played > 0 ? Math.round((stat.won / stat.played) * 100) : 0;
    html += `
        <div class="difficulty-stat-row">
        <span class="diff-name">${diffNames[stat.difficulty] || stat.difficulty}</span>
        <div class="diff-stats">
            <span class="diff-record">${stat.won}/${stat.played}</span>
            <span class="diff-winrate">${winRate}%</span>
            <span class="diff-avg">Avg: ${Math.round(stat.avg_guesses)} ${Math.round(stat.avg_guesses) === 1 ? 'guess' : 'guesses'}</span>
        </div>
        </div>
    `;
    });
    return html;
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
    const progressText = unlocked
        ? 'Completed'
        : progress.unit
            ? `${progress.current} / ${progress.target} ${progress.unit}`
            : 'Not completed';
    html += `
        <div class="achievement-card ${ach.category === 'clade' ? 'achievement-card-clade' : ''} ${unlocked ? 'achievement-unlocked' : 'achievement-locked'}">
        <div class="achievement-card-heading">
            <span class="achievement-medal" aria-hidden="true"></span>
            <div>
                ${ach.category === 'clade' ? '<div class="achievement-category">Clade collection</div>' : ''}
                <div class="achievement-title">${ach.name}</div>
            </div>
        </div>
        <div class="achievement-desc">${ach.desc}</div>
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
    return '<p style="color:var(--color-muted); font-style:italic; padding:20px; text-align:center;">No recent games.</p>';
    }

    const diffNames = {
    'muito_facil': 'Level I',
    'facil': 'Level II',
    'normal': 'Level III',
    'dificil': 'Level IV',
    'muito_dificil': 'Level V'
    };

    const today = getTodayString();

    let html = '';
    recentGames.forEach(game => {
    const date = new Date(game.created_at).toLocaleDateString();
    
    const isToday = game.played_date === today;
    const spoiler = isToday && !game.won;
    const dinoDisplay = spoiler 
        ? '<span style="color:var(--border-subtle); font-style:italic;">[ today\'s answer is hidden ]</span>' 
        : `<span style="color:var(--color-text-light); font-style:italic;">${game.target_dino}</span>`;

    html += `
        <div style="display:flex; justify-content:space-between; align-items:center; padding:15px; margin:8px 0; background:var(--bg-panel-darker); border-left:4px solid ${game.won ? 'var(--color-success)' : 'var(--border-base)'}; border-radius:4px;">
        <div style="flex:1;">
            <div style="display:flex; align-items:center; margin-bottom:5px;">
            <span style="color:${game.won ? '#c8e6c9' : '#d4a574'}; font-weight:700; margin-right:12px; font-size:1.1em;">${game.won ? '✓' : '…'}</span>
            ${dinoDisplay}
            </div>
        </div>
        <div style="text-align:right;">
            <div style="color:var(--color-accent); font-size:1em; font-weight:600; margin-bottom:3px;">${game.guess_count} ${game.guess_count === 1 ? 'guess' : 'guesses'}</div>
            <div style="color:var(--color-muted); font-size:0.8em;">${diffNames[game.difficulty]} • ${date}</div>
        </div>
        </div>
    `;
    });
    return html;
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
        description: 'A saurischian clade ancestrally characterized by bipedal locomotion. It includes ceratosaurs, tetanurans, and avialans.',
        achievementId: 'theropod_tracker',
        achievementClade: 'Theropoda',
        subclades: ['Ceratosauria', 'Tyrannosauroidea', 'Maniraptora']
    },
    {
        clade: 'Sauropodomorpha',
        title: 'Sauropodomorpha',
        description: 'A saurischian clade of predominantly herbivorous dinosaurs that includes early-diverging forms and Sauropoda.',
        achievementId: 'sauropod_collector',
        achievementClade: 'Sauropoda',
        subclades: ['Massopoda', 'Sauropoda', 'Macronaria']
    },
    {
        clade: 'Ornithischia',
        title: 'Ornithischia',
        description: 'A primarily herbivorous dinosaur clade diagnosed by features including the predentary bone and a retroverted pubis.',
        achievementId: 'ornithischian_explorer',
        achievementClade: 'Ornithischia',
        subclades: ['Thyreophora', 'Ornithopoda', 'Marginocephalia']
    }
];

let museumOverrideCatalogPromise = null;
let museumFallbackCatalogPromise = null;
let museumPaleodataCatalogPromise = null;
let museumLineageCatalogPromise = null;

async function ensureMuseumCatalogLineages(catalog) {
    const dinosaurs = Array.isArray(catalog) ? catalog : [];
    if (dinosaurs.every(dino => Array.isArray(dino?.linhagem) && dino.linhagem.length > 0)) {
        return dinosaurs;
    }

    if (!museumLineageCatalogPromise) {
        museumLineageCatalogPromise = fetch('phylosaur_db.json?v=atlas-1')
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
        const citation = escapeChallengeHtml(source?.citation || 'Scientific source');
        const url = getMuseumPaleodataSourceUrl(source?.url);
        return url
            ? `<a href="${escapeChallengeHtml(url)}" target="_blank" rel="noopener">${citation}</a>`
            : `<span>${citation}</span>`;
    }).join('<b>·</b>');

    return `
        <div class="museum-entry-paleo-sources">
            <strong>Reviewed sources</strong>
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
            ? `${formatMuseumAge(maxMa)}–${formatMuseumAge(minMa)} million years ago`
            : 'Numerical age not asserted';
    const countries = Array.isArray(record.countries) ? record.countries : [];
    const continents = Array.isArray(record.continents) ? record.continents : [];
    const formations = Array.isArray(record.formations) ? record.formations : [];
    const locationLabel = countries.length
        ? countries.join(' · ')
        : continents.length
            ? continents.join(' · ')
            : 'Discovery locations under review';

    return `
        <section class="museum-entry-paleodata" aria-label="Time and fossil locations">
            <div class="museum-entry-paleo-grid">
                <div class="museum-entry-paleo-card museum-entry-paleo-time">
                    <div class="museum-entry-paleo-label">When</div>
                    <strong>${escapeChallengeHtml(record.period || 'Geologic interval under review')}</strong>
                    <span>${escapeChallengeHtml(ageLabel)}</span>
                    ${hasAgeRange ? `
                        <div class="museum-time-scale"
                             role="img"
                             aria-label="${escapeChallengeHtml(record.period || 'Age range')}, ${escapeChallengeHtml(ageLabel)}">
                            <div class="museum-time-periods" aria-hidden="true">
                                <span>Triassic</span><span>Jurassic</span><span>Cretaceous</span>
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
                    <div class="museum-entry-paleo-label">Where fossils were found</div>
                    <strong>${escapeChallengeHtml(locationLabel)}</strong>
                    ${continents.length ? `
                        <div class="museum-paleo-chips">
                            ${continents.map(continent => `<span>${escapeChallengeHtml(continent)}</span>`).join('')}
                        </div>
                    ` : ''}
                    ${formations.length ? `
                        <div class="museum-paleo-formations">
                            <b>Rock units</b>
                            <span>${escapeChallengeHtml(formations.join(' · '))}</span>
                        </div>
                    ` : ''}
                </div>
            </div>
            <p class="museum-entry-paleo-note">
                Locations use modern geography and summarize reported fossil occurrences; they are not a reconstruction of ancient political or continental boundaries.
            </p>
            ${record.verification?.scope ? `
                <p class="museum-entry-paleo-scope">
                    <strong>Reviewed scope:</strong> ${escapeChallengeHtml(record.verification.scope)}
                </p>
            ` : ''}
            ${renderMuseumPaleodataSources(record)}
        </section>
    `;
}

async function loadMuseumOverrideCatalog() {
    if (!museumOverrideCatalogPromise) {
        museumOverrideCatalogPromise = fetch('phylosaur_media_overrides.json?v=8')
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
    let cache = JSON.parse(localStorage.getItem('phylosaur-image-cache-v5') || '{}');
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
            localStorage.setItem('phylosaur-image-cache-v5', JSON.stringify(cache));
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
        localStorage.setItem('phylosaur-image-cache-v5', JSON.stringify(cache));
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
        localStorage.setItem('phylosaur-image-cache-v5', JSON.stringify(cache));
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

function stopMuseumCardMediaLoading() {
    museumMediaGeneration += 1;
    museumMediaObserver?.disconnect();
    museumMediaObserver = null;
    museumMediaLoadQueue = [];
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
    return date.toLocaleDateString(undefined, {
        year: 'numeric',
        month: 'short',
        day: 'numeric'
    });
}

function getMuseumDiscoverySummary(record) {
    if (!record) {
        return {
            firstLabel: 'Unlock date unavailable',
            countLabel: 'Unlocked once',
            lastLabel: ''
        };
    }

    const firstDate = formatMuseumDiscoveryDate(record.firstDiscoveredAt);
    const lastDate = formatMuseumDiscoveryDate(record.lastDiscoveredAt);
    const firstLabel = record.firstDateUnknown
        ? 'Unlocked before date tracking'
        : firstDate
            ? `First unlocked ${firstDate}`
            : 'Unlock date unavailable';

    return {
        firstLabel,
        countLabel: record.count === 1
            ? 'Unlocked once'
            : `Unlocked ${record.count} times`,
        lastLabel: record.count > 1 && lastDate
            ? `Last unlocked ${lastDate}`
            : ''
    };
}

function getMuseumMediaCredit(name, media) {
    if (!media) {
        return '<span>No illustration is currently available for this entry.</span>';
    }

    if (media.source === 'wikimedia' || media.source === 'dinopedia') {
        const license = media.license_url
            ? `<a href="${media.license_url}" target="_blank" rel="noopener">${media.license}</a>`
            : media.license;
        const sourceName = media.source === 'dinopedia' ? 'Dinopedia' : 'Wikimedia Commons';
        const contributor = media.artist || `${sourceName} contributor`;
        const editorialNote = media.editorial_note
            ? `<span class="museum-entry-media-note">${escapeChallengeHtml(media.editorial_note)}</span>`
            : '';
        return `
            Image by ${contributor} · ${license}
            · <a href="${media.file_page}" target="_blank" rel="noopener">${sourceName}</a>
            ${editorialNote}
        `;
    }

    const commonsPage = `https://commons.wikimedia.org/wiki/File:${encodeURIComponent(name + ' TD.png')}`;
    return `
        Image source: <a href="${commonsPage}" target="_blank" rel="noopener">Wikimedia Commons</a>
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
        <button class="museum-image-viewer-close" type="button" aria-label="Close image">×</button>
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
        <article class="museum-entry-dialog" role="dialog" aria-modal="true" aria-label="${name}">
            <button class="museum-entry-close" type="button" onclick="dismissMuseumEntry()" aria-label="Close">×</button>
            <div class="museum-entry-loading">Opening ${name}…</div>
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
                <button class="museum-entry-close" type="button" onclick="dismissMuseumEntry()" aria-label="Close">×</button>
                <div class="museum-entry-loading" style="color:var(--color-danger);">
                    Could not open ${name}.<br>${error.message}
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
        muito_facil: 'Level I',
        facil: 'Level II',
        normal: 'Level III',
        dificil: 'Level IV',
        muito_dificil: 'Level V'
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
            <button class="museum-entry-close" type="button" onclick="dismissMuseumEntry()" aria-label="Close">×</button>

        <header class="museum-entry-header">
            <div class="museum-entry-kicker">Museum entry</div>
            <h2>${escapeChallengeHtml(name)}</h2>
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
                        aria-label="View larger image of ${name}">
                    <img src="dinosaur-footprint-1-svgrepo-com.svg" alt="${name}">
                </button>
                <figcaption>Loading illustration…</figcaption>
            </figure>

            <section class="museum-entry-copy">
                <div class="museum-entry-ornament">◆</div>
                <p class="museum-entry-description">
                    Loading encyclopedia overview…
                </p>

                <div class="museum-entry-paleodata-slot">
                    <div class="museum-entry-section-loading">Loading fossil record…</div>
                </div>

                <h3>Classification</h3>
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
            : 'No reviewed illustration is available yet.';
        image.src = media?.url || 'dinosaur-footprint-1-svgrepo-com.svg';
        button.disabled = !hasImage;
        if (hasImage) button.insertAdjacentHTML('beforeend', '<span>Click to enlarge</span>');
        caption.innerHTML = credit;
        activeMuseumEntryMedia = { name, url: media?.url || null, credit };
    }).catch(error => {
        console.warn(`Museum illustration unavailable for ${name}:`, error);
        const caption = overlay.querySelector('.museum-entry-figure figcaption');
        if (caption) caption.textContent = 'No reviewed illustration is available yet.';
    });

    void wikiPromise.then(wikiInfo => {
        if (!overlay.isConnected) return;
        const description = overlay.querySelector('.museum-entry-description');
        if (description) {
            description.textContent = wikiInfo?.description
                || 'No encyclopedia summary is available for this genus yet.';
        }

        const readMoreSlot = overlay.querySelector('.museum-entry-read-more-slot');
        if (readMoreSlot && wikiInfo?.url) {
            readMoreSlot.innerHTML = `
                <a class="museum-entry-read-more" href="${escapeChallengeHtml(wikiInfo.url)}"
                   target="_blank" rel="noopener">
                    <span>Read the full Wikipedia article</span>
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
        achievementName: achievementDefinition?.name || 'Collection milestone',
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
                    <h3 id="museum-atlas-title">Taxonomic overview</h3>
                    <p>
                        Museum specimens are organized here by three broad clades represented in the current
                        classification. The totals follow the stored lineage of each genus.
                    </p>
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
                        <span title="${subclade.unlocked} of ${subclade.total} discovered">
                            ${escapeChallengeHtml(subclade.clade)}
                            <small>${subclade.unlocked}/${subclade.total}</small>
                        </span>
                    `).join('');
                    return `
                        <button class="museum-atlas-card museum-atlas-${collection.clade.toLowerCase()}"
                                type="button"
                                onclick="openMuseumClade('${collection.clade}')"
                                aria-label="Explore ${collection.title}: ${collection.unlocked} of ${collection.total} genera discovered">
                            <strong class="museum-atlas-card-title">${collection.title}</strong>
                            <span class="museum-atlas-card-description">${collection.description}</span>

                            <span class="museum-atlas-count">
                                <strong>${collection.unlocked}</strong>
                                <span>of ${collection.total} discovered</span>
                                <small>${collection.percent}% of this branch</small>
                            </span>
                            <span class="museum-atlas-progress" aria-hidden="true">
                                <span style="width:${collection.percent}%"></span>
                            </span>

                            <span class="museum-atlas-subclades-label">Selected subordinate clades</span>
                            <span class="museum-atlas-subclades">${subclades}</span>

                            <span class="museum-atlas-achievement ${collection.achievementComplete ? 'is-complete' : ''}">
                                <span>${escapeChallengeHtml(collection.achievementName)}</span>
                                <strong>${achievementCurrent}/${collection.achievementTarget}</strong>
                                <i><span style="width:${achievementPercent}%"></span></i>
                            </span>

                            <span class="museum-atlas-open">Filter specimens by this clade</span>
                        </button>
                    `;
                }).join('')}
            </div>

            <p class="museum-atlas-note">
                Herrerasaurus, Sanjuansaurus, and Staurikosaurus are retained outside these three collections because
                their stored lineages do not place them within Theropoda, Sauropodomorpha, or Ornithischia.
            </p>
        </section>
    `;
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
        applyMuseumFilters();
        initializeMuseumCardMediaLoading();
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
    
    appContent.innerHTML = `<div class="game-card">${renderAppState('Loading Museum…')}</div>`;
    
    try {
        if (!fullDatabase || fullDatabase.length === 0) {
            const catalog = await callGameApi('catalog');
            fullDatabase = catalog.dinosaurs || [];
        }
        fullDatabase = await ensureMuseumCatalogLineages(fullDatabase);

        museumDiscoveryRecords = await getDiscoveryRecords();
        if (!currentUserId) synchronizeGuestAchievements();
        const unlockedList = Object.values(museumDiscoveryRecords)
            .map(record => record.name);
        const unlockedSet = new Set(unlockedList.map(name => name.toLowerCase()));

        const museumDinos = [...fullDatabase]
            .sort((a, b) => a.nome.localeCompare(b.nome));
        
        const totalCount = fullDatabase.length;
        const totalUnlocked = fullDatabase
            .filter(dino => unlockedSet.has(dino.nome.toLowerCase())).length;
        const totalPercent = totalCount > 0 ? Math.round((totalUnlocked / totalCount) * 100) : 0;

        let html = `
            <div class="game-card">
                <h2 class="screen-title">Museum</h2>
                
                <div class="museum-progress-container">
                    <div style="font-size:1.1em; color:var(--color-secondary); font-weight:600;">
                        Unlocked: ${totalUnlocked} / ${totalCount} (${totalPercent}%)
                    </div>
                    <div class="museum-progress-bar">
                        <div class="museum-progress-fill" style="width: ${totalPercent}%;"></div>
                    </div>
                    <div style="font-size:0.85em; color:var(--color-muted); font-style:italic;">
                        Complete challenges or practice games to unlock dinosaurs.
                    </div>
                </div>

                <div class="museum-view-switch" role="tablist" aria-label="Museum view">
                    <button type="button" role="tab" data-museum-view="atlas"
                            class="${museumView === 'atlas' ? 'active' : ''}"
                            aria-selected="${museumView === 'atlas'}"
                            aria-controls="museum-atlas-panel"
                            onclick="switchMuseumView('atlas')">Clade Atlas</button>
                    <button type="button" role="tab" data-museum-view="specimens"
                            class="${museumView === 'specimens' ? 'active' : ''}"
                            aria-selected="${museumView === 'specimens'}"
                            aria-controls="museum-specimens-panel"
                            onclick="switchMuseumView('specimens')">Specimens</button>
                </div>

                <div id="museum-atlas-panel" role="tabpanel" ${museumView === 'atlas' ? '' : 'hidden'}>
                    ${renderMuseumAtlas(unlockedSet)}
                </div>

                <div id="museum-specimens-panel" role="tabpanel" ${museumView === 'specimens' ? '' : 'hidden'}>
                <div class="museum-clade-filter" id="museum-clade-filter" ${selectedMuseumClade === 'all' ? 'hidden' : ''}>
                    <span>Exploring <strong id="museum-clade-filter-name">${escapeChallengeHtml(selectedMuseumClade === 'all' ? '' : selectedMuseumClade)}</strong></span>
                    <button type="button" onclick="clearMuseumCladeFilter()">Show all clades</button>
                </div>

                <div class="museum-toolbar">
                    <label class="museum-search" for="museum-search-input">
                        <span>Search the collection</span>
                        <input id="museum-search-input" type="search"
                               value="${escapeChallengeHtml(museumSearchQuery)}"
                               placeholder="Search by genus…"
                               autocomplete="off"
                               oninput="updateMuseumSearch(this.value)">
                    </label>

                    <div class="tab-row museum-tabs" role="group" aria-label="Filter Museum by level">
                        <button class="tab-btn museum-filter-all ${selectedMuseumLevel === 'all' ? 'active' : ''}"
                                data-museum-filter="all" aria-pressed="${selectedMuseumLevel === 'all'}"
                                onclick="switchMuseumLevel('all')">All</button>
                        <button class="tab-btn museum-filter-very-easy ${selectedMuseumLevel === 'muito_facil' ? 'active' : ''}"
                                data-museum-filter="muito_facil" aria-pressed="${selectedMuseumLevel === 'muito_facil'}"
                                onclick="switchMuseumLevel('muito_facil')">Level I</button>
                        <button class="tab-btn museum-filter-easy ${selectedMuseumLevel === 'facil' ? 'active' : ''}"
                                data-museum-filter="facil" aria-pressed="${selectedMuseumLevel === 'facil'}"
                                onclick="switchMuseumLevel('facil')">Level II</button>
                        <button class="tab-btn museum-filter-normal ${selectedMuseumLevel === 'normal' ? 'active' : ''}"
                                data-museum-filter="normal" aria-pressed="${selectedMuseumLevel === 'normal'}"
                                onclick="switchMuseumLevel('normal')">Level III</button>
                        <button class="tab-btn museum-filter-hard ${selectedMuseumLevel === 'dificil' ? 'active' : ''}"
                                data-museum-filter="dificil" aria-pressed="${selectedMuseumLevel === 'dificil'}"
                                onclick="switchMuseumLevel('dificil')">Level IV</button>
                        <button class="tab-btn museum-filter-very-hard ${selectedMuseumLevel === 'muito_dificil' ? 'active' : ''}"
                                data-museum-filter="muito_dificil" aria-pressed="${selectedMuseumLevel === 'muito_dificil'}"
                                onclick="switchMuseumLevel('muito_dificil')">Level V</button>
                    </div>
                </div>

                <div class="museum-filter-summary" id="museum-filter-summary" aria-live="polite">
                    Showing ${totalCount} specimens · ${totalUnlocked} unlocked
                </div>

                <div class="museum-grid">
        `;

        museumDinos.forEach(dino => {
            const isUnlocked = unlockedSet.has(dino.nome.toLowerCase());
            const lastClade = dino.terminalClade || dino.linhagem?.at(-1) || 'Dinosauria';
            const lineageData = Array.isArray(dino.linhagem) ? dino.linhagem.join('|') : '';
            const cardData = `data-museum-level="${dino.dificuldade}" data-museum-name="${escapeChallengeHtml(dino.nome.toLowerCase())}" data-museum-unlocked="${isUnlocked}" data-museum-lineage="${escapeChallengeHtml(lineageData)}"`;

            if (isUnlocked) {
                const discovery = getMuseumDiscoverySummary(
                    museumDiscoveryRecords[dino.nome.toLowerCase()]
                );
                html += `
                    <div class="museum-card unlocked difficulty-${DIFFICULTY_MAP[dino.dificuldade]}" ${cardData} role="button" tabindex="0"
                         aria-label="Open museum entry for ${dino.nome}"
                         onclick="showMuseumEntry('${dino.nome}')"
                        onkeydown="if(event.key === 'Enter' || event.key === ' '){ event.preventDefault(); showMuseumEntry('${dino.nome}'); }"
                         style="cursor:pointer;">
                        <div class="museum-card-art-container">
                            <img class="museum-card-art"
                                 data-museum-media-name="${escapeChallengeHtml(dino.nome)}"
                                 src="dinosaur-footprint-1-svgrepo-com.svg"
                                 alt="${dino.nome}" loading="lazy" decoding="async" />
                        </div>
                        <div class="museum-card-name">${dino.nome}</div>
                        <div class="museum-card-clade">${lastClade}</div>
                        <div class="museum-card-discovery">
                            <span>${discovery.firstLabel}</span>
                            ${museumDiscoveryRecords[dino.nome.toLowerCase()]?.count > 1
                                ? `<strong>${discovery.countLabel}</strong>`
                                : ''}
                        </div>
                        <div class="museum-card-source"></div>
                    </div>
                `;
            } else {
                html += `
                    <div class="museum-card locked difficulty-${DIFFICULTY_MAP[dino.dificuldade]}" ${cardData}>
                        <div class="museum-card-art-container">
                            <span class="museum-card-lock-icon" aria-hidden="true"></span>
                        </div>
                        <div class="museum-card-name">???</div>
                        <div class="museum-card-clade">Locked</div>
                    </div>
                `;
            }
        });

        html += `
                    <div class="museum-empty-state" id="museum-empty-state" hidden>
                        No specimens match this search and level filter.
                    </div>
                </div>
                </div>
            </div>
            <div id="clade-info"></div>`;
        appContent.innerHTML = html;

        updateMuseumCladeFilterDisplay();
        switchMuseumView(museumView);

    } catch (err) {
        console.error('Museum Error:', err);
        appContent.innerHTML = `<div class="game-card">${renderAppState('Could not load Museum.', {
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
    applyMuseumFilters();
}

function applyMuseumFilters() {
    const cards = [...document.querySelectorAll('.museum-card[data-museum-level]')];
    if (cards.length === 0) return;

    let visibleCount = 0;
    let visibleUnlocked = 0;

    cards.forEach(card => {
        const matchesLevel = selectedMuseumLevel === 'all'
            || card.dataset.museumLevel === selectedMuseumLevel;
        const matchesSearch = !museumSearchQuery
            || card.dataset.museumName.includes(museumSearchQuery);
        const matchesClade = selectedMuseumClade === 'all'
            || String(card.dataset.museumLineage || '').split('|').includes(selectedMuseumClade);
        const isVisible = matchesLevel && matchesSearch && matchesClade;

        card.hidden = !isVisible;
        if (!isVisible) return;

        visibleCount += 1;
        if (card.dataset.museumUnlocked === 'true') visibleUnlocked += 1;
    });

    const summary = document.getElementById('museum-filter-summary');
    if (summary) {
        const specimenLabel = visibleCount === 1 ? 'specimen' : 'specimens';
        const cladeLabel = selectedMuseumClade === 'all' ? '' : ` in ${selectedMuseumClade}`;
        summary.textContent = `Showing ${visibleCount} ${specimenLabel}${cladeLabel} · ${visibleUnlocked} unlocked`;
    }

    const emptyState = document.getElementById('museum-empty-state');
    if (emptyState) {
        emptyState.hidden = visibleCount !== 0;
        emptyState.textContent = selectedMuseumClade === 'all'
            ? 'No specimens match this search and level filter.'
            : `No ${selectedMuseumClade} specimens match these filters.`;
    }
}

function analyticsLabel(value) {
    const labels = {
        daily: 'Daily', practice: 'Practice', challenge: 'Friends',
        muito_facil: 'Level I', facil: 'Level II', normal: 'Level III',
        dificil: 'Level IV', muito_dificil: 'Level V',
        challenge_created: 'Challenge created', challenge_joined: 'Challenge joined',
        museum_opened: 'Museum entry viewed', game_started: 'Game started',
        game_won: 'Game won', game_gave_up: 'Game abandoned', hint_used: 'Hint used'
    };
    return labels[value] || String(value || 'Unknown');
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
        appContent.innerHTML = '<div class="game-card empty-state">Analytics access is restricted.</div>';
        return;
    }

    appContent.innerHTML = `<div class="game-card">${renderAppState('Loading private analytics…')}</div>`;

    let data;
    try {
        data = await callGameApi('analytics_dashboard', { days });
    } catch (error) {
        appContent.innerHTML = `<div class="game-card">${renderAppState('Could not load analytics.', {
            type: 'error', detail: error.message
        })}</div>`;
        return;
    }

    const summary = data.summary || {};
    const maxStarted = Math.max(1, ...data.byDay.map(day => Number(day.started || 0)));
    const chart = data.byDay.map(day => {
        const height = Math.max(3, Math.round((Number(day.started || 0) / maxStarted) * 100));
        const date = new Date(`${day.date}T00:00:00Z`).toLocaleDateString(undefined, { month: 'short', day: 'numeric', timeZone: 'UTC' });
        return `<div class="analytics-chart-column" title="${escapeChallengeHtml(date)}: ${day.started} games, ${day.visitors} visitors">
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
            <span>${completionRate}% complete</span>
        </div>`;
    }).join('');

    const modeRows = Object.entries(data.byMode || {}).map(([mode, count]) => `
        <div class="analytics-breakdown-row"><span>${escapeChallengeHtml(analyticsLabel(mode))}</span><strong>${count}</strong><span>sessions</span></div>
    `).join('');

    const activityRows = (data.recentActivity || []).map(event => {
        const when = new Date(event.createdAt).toLocaleString();
        const context = [
            event.player ? `@${event.player}` : '',
            analyticsLabel(event.mode),
            analyticsLabel(event.difficulty)
        ].filter(value => value && value !== 'Unknown').join(' · ');
        return `<div class="analytics-activity-row">
            <span>◆</span>
            <div><strong>${escapeChallengeHtml(analyticsLabel(event.type))}</strong>${context ? `<small>${escapeChallengeHtml(context)}</small>` : ''}</div>
            <time>${escapeChallengeHtml(when)}</time>
        </div>`;
    }).join('');

    const playerRows = (data.registeredPlayers || []).map((player, index) => {
        const lastPlayed = player.lastPlayed
            ? new Date(`${player.lastPlayed}T00:00:00`).toLocaleDateString()
            : 'Never';
        return `<div class="analytics-player-row">
            <span class="analytics-player-rank">${index + 1}</span>
            <strong>@${escapeChallengeHtml(player.username)}</strong>
            <span>${player.gamesPlayed} games · ${player.gamesWon} wins (${player.winRate}%)</span>
            <span>Current ${player.currentStreak} · Best ${player.bestStreak}</span>
            <time>Last daily: ${escapeChallengeHtml(lastPlayed)}</time>
        </div>`;
    }).join('');

    const pagination = data.pagination || {};
    const hasTruncatedData = Object.values(pagination).some(Boolean);

    appContent.innerHTML = `
    <div class="game-card analytics-dashboard">
        <div class="analytics-header">
            <div>
                <div class="friends-kicker">Private Analytics</div>
                <h2>Phylosaur Analytics</h2>
                <p>Private administrator view. Registered usernames may be displayed; emails, IP addresses and fingerprints are never included.</p>
            </div>
            <div class="analytics-range" role="group" aria-label="Analytics period">
                ${[7, 30, 90].map(period => `<button class="btn-hint btn-header ${period === data.days ? 'active' : ''}" onclick="showAnalyticsDashboard(${period})">${period}d</button>`).join('')}
            </div>
        </div>

        <div class="analytics-metrics">
            ${analyticsMetricCard('Unique visitors', summary.uniqueVisitors, `${summary.untrackedSessions || 0} older sessions untracked`)}
            ${analyticsMetricCard('Games started', summary.totalSessions)}
            ${analyticsMetricCard('Games completed', summary.completedGames, `${summary.completionRate}% completion`)}
            ${analyticsMetricCard('Wins', summary.wins, `${summary.winRate}% of completed games`)}
            ${analyticsMetricCard('Average guesses', summary.averageGuesses)}
            ${analyticsMetricCard('Average hints', summary.averageHints)}
            ${analyticsMetricCard('New accounts', summary.newAccounts)}
            ${analyticsMetricCard('Registered accounts', summary.registeredAccounts, `${summary.activeRegisteredPlayers || 0} with recorded games`)}
            ${analyticsMetricCard('Highest streak', summary.highestBestStreak, summary.highestStreakPlayer ? `@${summary.highestStreakPlayer}` : 'No streak recorded')}
            ${analyticsMetricCard('Anonymous sessions', summary.anonymousSessions)}
            ${analyticsMetricCard('Friend challenges', summary.challengesCreated, `${summary.challengeJoins} joins`)}
            ${analyticsMetricCard('Museum views', summary.museumViews)}
        </div>

        <section class="analytics-section">
            <h3>Registered players · highest streaks</h3>
            <div class="analytics-players">${playerRows || '<p class="empty-state">No registered player statistics yet.</p>'}</div>
            ${(data.registeredPlayers || []).length >= 100
                ? '<p class="analytics-section-note">Showing the first 100 accounts, ordered by best streak and activity.</p>'
                : ''}
        </section>

        <section class="analytics-section">
            <h3>Games by day</h3>
            <div class="analytics-chart">${chart}</div>
        </section>

        <div class="analytics-two-column">
            <section class="analytics-section">
                <h3>By level</h3>
                <div class="analytics-breakdown">${difficultyRows || '<p class="empty-state">No games in this period.</p>'}</div>
            </section>
            <section class="analytics-section">
                <h3>By mode</h3>
                <div class="analytics-breakdown">${modeRows || '<p class="empty-state">No games in this period.</p>'}</div>
            </section>
        </div>

        <section class="analytics-section">
            <h3>Recent activity</h3>
            <div class="analytics-activity">${activityRows || '<p class="empty-state">New tracked events will appear here.</p>'}</div>
        </section>

        ${hasTruncatedData ? '<p class="analytics-data-warning">The safety limit was reached for at least one dataset. Some totals may be partial.</p>' : ''}
        <p class="analytics-generated">Generated ${escapeChallengeHtml(new Date(data.generatedAt).toLocaleString())}</p>
    </div>`;
}