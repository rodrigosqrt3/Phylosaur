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
    const [completionStatus] = await Promise.all([
        getDailyCompletionStatus(),
        initializeAnalyticsAccess()
    ]);
    setHeaderControls('difficulty');
    const appContent = document.getElementById('app-content');
    
    appContent.innerHTML = `            
        <div class="game-card" style="text-align:center;">
        <h2 style="color:var(--color-primary); margin-bottom:12px; font-size:1.8em;">Daily Challenge</h2>
        <p style="color:var(--color-secondary); margin-bottom:30px; font-size:0.95em; letter-spacing:1px;">
            ${getCurrentDateFormatted()} - Choose a level
        </p>
        <p style="color:#6b5340; font-size:0.82em; font-style:italic; letter-spacing:1px; margin-top:8px;">
        Next daily challenge in <span id="countdown-timer" style="color:var(--color-secondary); font-weight:600;">--:--:--</span>
        </p>

        <div style="display:flex; flex-wrap:wrap; justify-content:center; gap:20px; margin:30px 0;">
            ${generateDifficultyButton('muito_facil', 'LEVEL I', 'I', '', completionStatus.muito_facil)}
            ${generateDifficultyButton('facil', 'LEVEL II', 'II', '', completionStatus.facil)}
            ${generateDifficultyButton('normal', 'LEVEL III', 'III', '', completionStatus.normal)}
            ${generateDifficultyButton('dificil', 'LEVEL IV', 'IV', '', completionStatus.dificil)}
            ${generateDifficultyButton('muito_dificil', 'LEVEL V', 'V', '', completionStatus.muito_dificil)}
        </div>

        <div style="margin-top:30px; text-align:center;">
            <div style="display:flex; gap:15px; justify-content:center; flex-wrap:wrap;">
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
            <h2>Play with Friends</h2>
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
    <div class="game-card" style="text-align:center;">
        <h2 style="color:var(--color-primary); margin-bottom:12px; font-size:1.8em;">Practice Mode</h2>
        <p style="color:var(--color-secondary); margin-bottom:30px; font-size:0.95em; letter-spacing:1px;">
        Choose a level and play as often as you like
        </p>

        <div style="display:flex; flex-wrap:wrap; justify-content:center; gap:20px; margin:30px 0;">
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
            onclick="startPracticeChallenge('${difficulty}')" 
            style="padding:30px; font-size:1.2em; flex:0 1 260px;">
        <div style="font-weight:bold; margin-bottom:10px; font-size:1.3em; letter-spacing:3px;">
        ${name}
        </div>
        <div style="margin:8px 0;">
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
        ? '<div style="position:absolute; top:10px; right:10px; width:24px; height:24px; background:var(--color-success); border-radius:50%; display:flex; align-items:center; justify-content:center; font-size:14px; color:#fff; font-weight:bold;">✓</div>'
        : '';

    const borderClass = completed ? 'difficulty-completed' : '';

    return `
        <button class="difficulty-btn difficulty-${DIFFICULTY_MAP[difficulty]} ${borderClass}" 
                onclick="startDailyChallenge('${difficulty}')" 
                style="padding:30px; font-size:1.2em; position:relative; flex:0 1 260px;">
        ${statusIndicator}
        <div style="font-weight:bold; margin-bottom:10px; font-size:1.3em; letter-spacing:3px;">
            ${name}
        </div>
        <div style="margin:8px 0;">
            ${tiers}
        </div>
        ${description ? `<div style="font-size:0.75em; color:var(--color-muted); font-style:italic;">${description}</div>` : ''}
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
        <h2 style="color:var(--color-primary); margin-bottom:20px; text-align:center; font-size:2em; letter-spacing:3px;">
        STATISTICS
        </h2>

        <div style="text-align:center; margin-bottom:30px; color:var(--color-secondary); font-size:1.1em; letter-spacing:2px;">
        Player: <span style="color:var(--color-accent); font-weight:600;">${currentUser}</span>
        </div>

        <div class="stats" style="grid-template-columns: repeat(2, 1fr); margin-bottom:40px;">
        <div class="stat"><div class="stat-value">${gamesPlayed}</div><div class="stat-label">Games Played</div></div>
        <div class="stat"><div class="stat-value">${gamesWon}</div><div class="stat-label">Games Won</div></div>
        <div class="stat"><div class="stat-value">${winRate}%</div><div class="stat-label">Success Rate</div></div>
        <div class="stat"><div class="stat-value">${stats?.best_score || '-'}</div><div class="stat-label">Best Score</div></div>
        </div>

        ${generateStreakDisplay(streakData)}

        <div style="background:var(--bg-panel); padding:25px; border-radius:8px; margin-bottom:30px; border:2px solid var(--border-subtle);">
        <h3 style="color:var(--color-text-light); margin-bottom:20px; font-size:1.3em; border-bottom:2px solid var(--border-subtle); padding-bottom:12px;">Performance by Level</h3>
        ${generateDifficultyStats(diffStats)}
        </div>

        <div class="achievements-panel">
        <div class="achievements-heading">
            <h3>Achievements</h3>
            <span>${unlockedAchievementCount} / ${ACHIEVEMENT_DEFINITIONS.length} unlocked</span>
        </div>
        ${generateAchievements(unlockedAchievements, achievementProgress)}
        </div>

        <div style="background:var(--bg-panel); padding:25px; border-radius:8px; border:2px solid var(--border-subtle);">
        <h3 style="color:var(--color-text-light); margin-bottom:20px; font-size:1.3em; border-bottom:2px solid var(--border-subtle); padding-bottom:12px;">Recent Games</h3>
        ${generateRecentGames(recentGames)}
        </div>
    </div>
    `;
}

function generateStreakDisplay(streakData) {
    if (!currentUser) return '';

    if (!streakData || streakData.current === 0) {
    return `
        <div style="text-align:center; padding:30px; background:rgba(61,47,31,0.3); border-radius:8px; margin-bottom:30px; border:2px solid var(--border-subtle);">
        <div style="font-size:1.5em; color:var(--color-muted); margin-bottom:10px;">No streak yet</div>
        <div style="color:var(--color-secondary); font-size:0.95em;">Play a daily challenge to start one.</div>
        </div>
    `;
    }

    return `
    <div style="text-align:center; padding:30px; background:var(--color-warning-bg); border-radius:8px; margin-bottom:30px; border:2px solid var(--color-warning);">
        <div style="font-size:2.5em; color:var(--color-warning); margin-bottom:15px;">◆ ${streakData.current}</div>
        <div style="font-size:1.2em; color:var(--color-primary); margin-bottom:8px; font-weight:600; letter-spacing:1px;">CURRENT STREAK</div>
        <div style="color:var(--color-secondary); font-size:0.95em;">Best: ${streakData.best} days</div>
        <div style="margin-top:20px; color:var(--color-muted); font-size:0.85em; font-style:italic;">Last played: ${streakData.lastPlayed || 'Never'}</div>
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

function showAbout() {
    setAppRoute('/about');
    setHeaderControls('about');
    const appContent = document.getElementById('app-content');
    
    appContent.innerHTML = `

    
    <div class="game-card" style="max-width: 900px; margin: 0 auto;">
        <h2 style="color:var(--color-primary); margin-bottom:30px; text-align:center; font-size:2em; letter-spacing:3px;">
        About Phylosaur
        </h2>
        
        <div style="color:var(--color-text-light); line-height:1.85; font-size:1.02em; text-align:left;">

        <p style="margin-bottom:25px; font-style:italic; color:var(--color-secondary); font-size:0.95em;">
        <strong>Phylosaur</strong> is a deliberate meeting of language and subject. The scientific element 
        <em>phylo-</em>, from Greek <em>phylon</em> (a lineage, tribe, or related group), points to phylogeny: 
        the history and pattern of evolutionary relationships. Its sound also recalls <em>philo-</em>, from 
        Greek <em>philos</em>, associated with affection or love. <em>-saur</em>, from Greek <em>sauros</em> 
        (lizard), is the historical element familiar from <em>Dinosauria</em> and countless dinosaur names. 
        Phylosaur can therefore be read in two complementary ways: a game about dinosaur phylogeny, and a 
        small expression of love for dinosaurs. The resemblance to “lizard” belongs to the history of the 
        word, not to a claim that dinosaurs were merely lizards.
        </p>
        
        <p style="margin-bottom:25px;">
            Phylosaur is an independent educational game about the dinosaur family tree. A hidden genus must be 
            identified by comparing guesses across a branching classification. Each attempt reveals the deepest 
            named clade shared by the guess and the target. Gradually, familiar names become landmarks within a 
            larger evolutionary history, and classification becomes a process of navigation rather than memorization.
        </p>

        <h3 style="color:var(--color-accent); margin:35px 0 20px; font-size:1.4em; font-weight:400; letter-spacing:2px; border-bottom:2px solid var(--border-subtle); padding-bottom:12px;">
            Phylogenetic Foundations
        </h3>

        <p style="margin-bottom:25px;">
            Modern biological classification rests upon phylogenetic systematics, a methodology that organizes 
            organisms according to their evolutionary relationships rather than superficial morphological 
            similarities. This approach, pioneered by Willi Hennig in the mid-twentieth century, recognizes 
            that meaningful taxonomic groupings (clades) must be monophyletic, comprising an ancestor and all 
            of its descendants. Each clade is diagnosed by the presence of synapomorphies, which are derived 
            character states inherited from a common ancestor and shared exclusively among its descendants.
        </p>

        <p style="margin-bottom:25px;">
            Within this framework, Dinosauria represents a well-supported clade united by numerous 
            osteological synapomorphies. The group is formally diagnosed by features including an open 
            acetabulum, elongated deltopectoral crest on the humerus, and modifications to the ankle joint 
            that facilitate a parasagittal gait. Dinosauria is subdivided into two primary clades: Saurischia, 
            characterized by a triradiate pelvis with the pubis directed anteroventrally, and Ornithischia, 
            distinguished by a retroversed pubis and predentary bone. Modern phylogenetic analyses place 
            Avialae (birds) within Saurischia as highly derived theropods, rendering traditional distinctions 
            between "dinosaurs" and "birds" paraphyletic and therefore scientifically obsolete.
        </p>

        <p style="margin-bottom:25px; padding:20px; background:rgba(61,47,31,0.3); border-left:4px solid var(--color-muted); font-style:italic;">
        "The combination of such characters... will, it is presumed, be deemed sufficient ground for establishing 
        a distinct tribe or sub-order of Saurian Reptiles, for which I would propose the name of <em>Dinosauria</em>."
        <span style="display:block; margin-top:10px; font-style:normal; font-size:0.9em; color:var(--color-secondary);">
            - Richard Owen (1842). Report on British Fossil Reptiles, Part II. <em>Report of the British Association 
            for the Advancement of Science</em>, 11: 60–204.
        </span>
        </p>

        <h3 style="color:var(--color-accent); margin:35px 0 20px; font-size:1.4em; font-weight:400; letter-spacing:2px; border-bottom:2px solid var(--border-subtle); padding-bottom:12px;">
            Game Mechanics and Educational Objectives
        </h3>

        <p style="margin-bottom:25px;">
            Players are presented with a mystery taxon and must infer its phylogenetic position through 
            iterative hypothesis testing. Each classification attempt reveals the deepest node shared between 
            the proposed taxon and the target, thereby progressively constraining the phylogenetic search 
            space. This mechanism mirrors the actual process of phylogenetic inference, wherein researchers 
            place unknown taxa by identifying their closest relatives through character analysis and cladistic 
            methodology.
        </p>

        <p style="margin-bottom:25px;">
        The game offers five difficulty tiers determined by taxonomic familiarity as measured through 
        Wikipedia pageview statistics over a thirty-day period. This metric serves as a proxy for public 
        recognition rather than phylogenetic complexity or fossil completeness. Higher difficulty levels 
        incorporate taxa with lower pageview counts (generally lesser-known genera that receive less popular 
        attention) thereby requiring broader taxonomic knowledge independent of their scientific importance 
        or systematic resolution. Daily challenges are generated from the date and selected level, so a given level 
        presents the same target throughout the player's local calendar day.
        </p>

        <h3 style="color:var(--color-accent); margin:35px 0 20px; font-size:1.4em; font-weight:400; letter-spacing:2px; border-bottom:2px solid var(--border-subtle); padding-bottom:12px;">
            Data Sources and Taxonomic Curation
        </h3>

        <p style="margin-bottom:25px;">
        The genus catalogue began with open paleontological occurrence and taxonomic data, particularly records 
        assembled through the Paleobiology Database, followed by filtering and manual review. The present lineage 
        database combines the project's earlier curated classifications with lineages retrieved through 
        <em>taxodist</em> from The Taxonomicon. The two sources are not simply placed side by side: automated code 
        cleans placeholder ranks, detects ambiguous names and biological homonyms, removes playable genera used as 
        internal clades, and aligns older fallback paths to a shared taxonomic backbone.
        </p>

        <p style="margin-bottom:25px;">
        The hybrid-building process learns commonly supported paths from successfully resolved genera and uses those 
        paths to bring retained lineages into a coherent structure. Before an output is accepted, the database is 
        checked for repeated nodes, placeholder clades, incorrect lineage depths, playable genera appearing as 
        ancestors, and named clades assigned to more than one parent. When an automatic result is unresolved or 
        collapses too much useful structure, the curated lineage is retained for later investigation. This produces 
        a practical working tree while preserving a record of which placements came from each source.
        </p>

        <p style="margin-bottom:25px;">
            Difficulty classification uses thirty-day Wikipedia pageview totals retrieved through Wikimedia services. 
            Genera are distributed across five broad familiarity bands, with frequently visited names tending toward 
            lower levels and less familiar names toward higher ones. Homonyms require special attention because traffic 
            may refer to a plant, place, person, or unrelated animal. Pageviews measure public attention rather than 
            phylogenetic complexity, fossil completeness, or scientific importance.
        </p>

        <p style="margin-bottom:25px;">
            Museum summaries are requested from Wikipedia for accessible orientation. Illustrations come from 
            reusable files identified through Wikimedia Commons or explicitly licensed Dinopedia pages. 
            Existing reviewed choices are preserved, while new fallback images must pass checks for subject relevance, 
            file type, reusable licensing, attribution, and minimum quality. Creator, source, and licence information 
            accompanies each available image so that visitors can follow it back to its original record.
        </p>

        <h3 style="color:var(--color-accent); margin:35px 0 20px; font-size:1.4em; font-weight:400; letter-spacing:2px; border-bottom:2px solid var(--border-subtle); padding-bottom:12px;">
            Mathematical Foundations
        </h3>
        <p style="margin-bottom:25px;">
            The proximity measure computed by Phylosaur has a precise mathematical interpretation. Let $L_A$ be 
            the ordered lineage of a guessed genus $A$, and let $L_B$ be the lineage of the hidden target $B$. Both 
            sequences begin at <em>Dinosauria</em> and proceed toward increasingly specific clades. The game counts 
            how many consecutive entries they share before diverging. If their longest common prefix has length 
            $m(A,B)$, then:
        </p>
        <p style="margin-bottom:25px; padding:20px; background:rgba(61,47,31,0.3); border-radius:4px; text-align:center; overflow-x:auto; -webkit-overflow-scrolling:touch;">
            $$m(A,B)=\\max\\left\\{k: L_A[i]=L_B[i]\\;\\text{for every}\\;0\\le i&lt;k\\right\\}$$
        </p>
        <p style="margin-bottom:25px;">
            The final clade inside this common prefix is the deepest named common ancestor represented by the game. 
            The displayed proximity score is the shared-prefix length divided by the depth of the target:
        </p>
        <p style="margin-bottom:25px; padding:20px; background:rgba(61,47,31,0.3); border-radius:4px; text-align:center; overflow-x:auto; -webkit-overflow-scrolling:touch;">
            $$s(A,B)=100\\times\\frac{m(A,B)}{|L_B|}$$
        </p>
        <p style="margin-bottom:25px;">
            This percentage answers a direct game question: how much of the target's stored lineage has the guess 
            followed successfully? Because the denominator is $|L_B|$, the target's depth, the score need not be 
            symmetric when two genera have lineages of different lengths. Two distinct genera can occasionally share 
            every stored clade and reach 100% proximity; the exact genus name is still required to win.
        </p>
        <p style="margin-bottom:25px;">
            A related structural separation can be defined by treating every stored parent–child step as one unit:
        </p>
        <p style="margin-bottom:25px; padding:20px; background:rgba(61,47,31,0.3); border-radius:4px; text-align:center; overflow-x:auto; -webkit-overflow-scrolling:touch;">
            $$\\Delta(A,B)=|L_A|+|L_B|-2m(A,B)$$
        </p>
        <p style="margin-bottom:25px;">
            This is not the percentage used during play, but it expresses how many stored steps separate the two 
            lineage paths after their divergence. Named clades are not evenly spaced units of time, anatomy, or 
            evolutionary change, so neither $s$ nor $\\Delta$ should be interpreted as geological duration or 
            morphological difference.
        </p>
        <p style="margin-bottom:25px;">
            From an information-theoretic perspective, each guess is a query that reveals the depth and identity of a 
            shared branch. The revealed clade restricts the remaining candidates to genera compatible with that portion 
            of the target path. Hints reduce uncertainty more directly by exposing the next clade or, after the stored 
            lineage is exhausted, a clue about the target name. The waiting rule between hints creates space for the 
            player to use the new information. Dinosaur branches are highly uneven: 
            one clade may contain hundreds of candidates while another contains only a few. Consequently, different 
            guesses can provide very different amounts of information, which is an important part of the strategy.
        </p>
        <p style="margin-bottom:25px;">
            Difficulty stratification has its own statistical basis. Let $V=\\{v_1,v_2,\\ldots,v_n\\}$ be the 
            sorted vector of usable thirty-day pageview totals. Boundaries near the 20th, 40th, 60th, and 80th 
            percentiles divide the observed distribution into five familiarity bands. These are empirical game-design 
            categories, shaped by language, public interest, media attention, and the collection period. Genera without 
            reliable traffic data - including ambiguous homonyms - are handled conservatively.
        </p>

        <h3 style="color:var(--color-accent); margin:35px 0 20px; font-size:1.4em; font-weight:400; letter-spacing:2px; border-bottom:2px solid var(--border-subtle); padding-bottom:12px;">
            Methodological Considerations and Limitations
        </h3>

        <p style="margin-bottom:25px;">
            Phylogenetic trees are hypotheses of evolutionary relationships, not immutable facts. The topology 
            presented here represents one interpretation of available data and may differ from alternative 
            analyses that employ different character matrices, taxon sampling strategies, or analytical methods. 
            Furthermore, phylogenetic resolution varies considerably across the dinosaurian tree; while some 
            clades such as Tyrannosauridae exhibit robust support across multiple independent analyses, others 
            remain poorly resolved due to incomplete fossil records or morphological conservatism. Users should 
            interpret tree topology with appropriate epistemic humility, recognizing that scientific understanding 
            of dinosaurian phylogeny continues to evolve.
        </p>

        <p style="margin-bottom:25px;">
            Taxonomic nomenclature follows standard conventions wherein genus names are italicized and capitalized, 
            while higher-level clade names are rendered in standard font with initial capitalization. Some taxa 
            included in the database may represent <em>nomina dubia</em> (names of doubtful validity based on 
            insufficient diagnostic material) or taxonomic wastebasket groups subject to ongoing systematic revision. 
        </p>

        <p style="margin-bottom:25px;">
            The game must express each genus as one ordered path, but published analyses may instead recover competing 
            positions or an unresolved polytomy. Synonyms create another difficulty: a historical genus may later be 
            absorbed into another, yet remain culturally or historically recognizable. Phylosaur may retain such names 
            when they contribute to the educational catalogue, while the review pipeline records unresolved placements 
            for future correction. The resulting tree is intentionally detailed, but its detail should not be confused 
            with equal certainty at every node.
        </p>

        <h3 style="color:var(--color-accent); margin:35px 0 20px; font-size:1.4em; font-weight:400; letter-spacing:2px; border-bottom:2px solid var(--border-subtle); padding-bottom:12px;">
            Acknowledgments and Further Resources
        </h3>

        <p style="margin-bottom:25px;">
            This project acknowledges the foundational work of countless paleontologists whose systematic research 
            underlies our current understanding of dinosaurian phylogeny. For readers seeking deeper engagement 
            with the primary literature, recommended resources include the Paleobiology Database (paleobiodb.org), 
            which archives stratigraphic and taxonomic occurrence data; the Tree of Life Web Project (tolweb.org), 
            providing comprehensive phylogenetic overviews; and Fossilworks (fossilworks.org), which maintains 
            detailed taxonomic and bibliographic records. Users interested in formal training in phylogenetic 
            methodology are encouraged to consult <em>Phylogenetics: Theory and Practice of Phylogenetic Systematics</em> 
            by E.O. Wiley and Bruce Lieberman, or <em>Inferring Phylogenies</em> by Joseph Felsenstein.
        </p>

        <div style="margin-bottom:30px; padding:22px; background:var(--bg-panel); border:1px solid var(--border-subtle); border-radius:4px;">
            <div style="color:var(--color-primary); margin-bottom:14px; font-size:0.9em; letter-spacing:2px; text-transform:uppercase;">
                Selected scientific landmarks
            </div>
            <ul style="padding-left:22px; color:var(--color-secondary); line-height:1.75;">
                <li style="margin-bottom:8px;">Owen, R. (1842). <em>Report on British Fossil Reptiles, Part II.</em></li>
                <li style="margin-bottom:8px;">Seeley, H. G. (1888). <em>On the Classification of the Fossil Animals Commonly Named Dinosauria.</em></li>
                <li style="margin-bottom:8px;">Hennig, W. (1966). <em>Phylogenetic Systematics.</em></li>
                <li style="margin-bottom:8px;">Padian, K. &amp; Chiappe, L. M. (1998). <em>The Origin and Early Evolution of Birds.</em></li>
                <li style="margin-bottom:8px;">Weishampel, D. B., Dodson, P. &amp; Osmólska, H., eds. (2004). <em>The Dinosauria</em>, 2nd ed.</li>
                <li>Nesbitt, S. J. (2011). <em>The Early Evolution of Archosaurs: Relationships and the Origin of Major Clades.</em></li>
            </ul>
        </div>

        <p style="margin-bottom:35px;">
            Phylosaur is provided as an educational resource without commercial intent. Users are encouraged to 
            provide feedback regarding taxonomic inaccuracies, interface improvements, or pedagogical suggestions 
            that might enhance the tool's effectiveness as an instructional aid. While this application aims for 
            scientific rigor, it should not be cited as a primary source in formal academic contexts; instead, 
            users should trace information to original systematic publications through the references provided in 
            supplementary materials.
        </p>

        </div>

        <p style="margin-bottom:35px; padding:20px; background:rgba(61,47,31,0.3); border-left:4px solid var(--color-muted); border-radius:2px;">
        Phylosaur is currently under active development. Taxonomic inaccuracies, interface issues, 
        missing genera, or any other problems are expected and your feedback is genuinely invaluable. 
        If you encounter anything worth reporting, however small, please write to 
        <a href="mailto:rodrigo03.villa@gmail.com" style="color:var(--color-accent); text-decoration:none; border-bottom:1px solid var(--color-muted);">rodrigo03.villa@gmail.com</a>. 
        Every message is read and deeply appreciated.
        </p>

        <button class="btn-new-game" onclick="showDifficultySelection()" style="margin-top:30px;">
        Return to Challenge Selection
        </button>
    </div>
    `;
    const aboutCard = appContent.querySelector('.game-card');
    ensureMathRenderer().then(() => {
        if (!aboutCard?.isConnected || !window.renderMathInElement) return;
        renderMathInElement(aboutCard, {
            delimiters: [
                { left: '$$', right: '$$', display: true },
                { left: '$', right: '$', display: false }
            ],
            throwOnError: false
        });
    }).catch(error => console.warn('Math renderer could not be loaded:', error));
}

async function showHowToPlay() {
    const action = await showModal({
    title: 'How to Play',
    message: `
        <div style="text-align:left; line-height:2;">
        <p style="margin-bottom:16px;">A mystery dinosaur is hidden each day. Your goal is to identify it using the clues revealed by each guess.</p>
        
        <div style="margin-bottom:12px; padding:12px; background:rgba(61,47,31,0.4); border-radius:4px;">
            <strong style="color:var(--color-primary);">1. Guess a genus</strong><br>
            Type any dinosaur name and submit. The tree will reveal how closely related your guess is to the target.
        </div>

        <div style="margin-bottom:12px; padding:12px; background:rgba(61,47,31,0.4); border-radius:4px;">
            <strong style="color:var(--color-primary);">2. Read the tree</strong><br>
            Each guess reveals the deepest clade shared with the mystery dinosaur. The closer on the tree, the warmer you are.
        </div>

        <div style="margin-bottom:12px; padding:12px; background:rgba(61,47,31,0.4); border-radius:4px;">
            <strong style="color:var(--color-primary);">3. Hints</strong><br>
            You have 3 hints per challenge. Hints reveal the next clade, then clues about the genus name once the full lineage is known. You must make 2 guesses between hints.
        </div>

        <div style="margin-bottom:12px; padding:12px; background:rgba(61,47,31,0.4); border-radius:4px;">
            <strong style="color:var(--color-primary);">4. Five difficulty levels</strong><br>
            Level I features well-known genera. Level V features obscure taxa requiring broad taxonomic knowledge.
        </div>

        <div style="padding:12px; background:rgba(61,47,31,0.4); border-radius:4px;">
            <strong style="color:var(--color-primary);">Tip</strong><br>
            Click any node on the tree to read about that clade. New here? Start with Level I.
            <br><br>
            <span style="color:var(--color-muted); font-style:italic; font-size:0.9em;">
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
        <div class="achievement-card ${unlocked ? 'achievement-unlocked' : 'achievement-locked'}">
        <div class="achievement-card-heading">
            <span class="achievement-medal" aria-hidden="true"></span>
            <div class="achievement-title">${ach.name}</div>
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

let museumOverrideCatalogPromise = null;
let museumFallbackCatalogPromise = null;

async function loadMuseumOverrideCatalog() {
    if (!museumOverrideCatalogPromise) {
        museumOverrideCatalogPromise = fetch('phylosaur_media_overrides.json?v=6')
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
        return `
            Image by ${contributor} · ${license}
            · <a href="${media.file_page}" target="_blank" rel="noopener">${sourceName}</a>
        `;
    }

    const commonsPage = `https://commons.wikimedia.org/wiki/File:${encodeURIComponent(name + ' TD.png')}`;
    return `
        Image source: <a href="${commonsPage}" target="_blank" rel="noopener">Wikimedia Commons</a>
    `;
}

function closeMuseumEntry() {
    document.getElementById('museum-image-viewer')?.remove();
    document.getElementById('museum-entry-overlay')?.remove();
    document.body.style.overflow = '';
    activeMuseumEntryMedia = null;

    if (museumEntryEscapeHandler) {
        document.removeEventListener('keydown', museumEntryEscapeHandler);
        museumEntryEscapeHandler = null;
    }
}

function dismissMuseumEntry() {
    closeMuseumEntry();
    if (getCurrentAppRoute().startsWith('/museum/')) {
        navigateBackOrHome('/museum');
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

    const [media, wikiInfo] = await Promise.all([
        getCachedDinoMedia(name),
        fetchWikipediaInfo(name)
    ]);

    if (!document.body.contains(overlay)) return;

    const imageUrl = media?.url || 'dinosaur-footprint-1-svgrepo-com.svg';
    const credit = getMuseumMediaCredit(name, media);
    const levelNames = {
        muito_facil: 'Level I',
        facil: 'Level II',
        normal: 'Level III',
        dificil: 'Level IV',
        muito_dificil: 'Level V'
    };
    const lineage = (dino.linhagem || [])
        .map(clade => `<span>${clade}</span>`)
        .join('<b>›</b>');
    const discovery = getMuseumDiscoverySummary(
        museumDiscoveryRecords[name.toLowerCase()]
    );

    activeMuseumEntryMedia = {
        name,
        url: media?.url || null,
        credit
    };

    overlay.querySelector('.museum-entry-dialog').innerHTML = `
            <button class="museum-entry-close" type="button" onclick="dismissMuseumEntry()" aria-label="Close">×</button>

        <header class="museum-entry-header">
            <div class="museum-entry-kicker">Museum entry</div>
            <h2>${name}</h2>
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
                        ${media?.url ? '' : 'disabled'}
                        aria-label="View larger image of ${name}">
                    <img src="${imageUrl}" alt="${name}">
                    ${media?.url ? '<span>Click to enlarge</span>' : ''}
                </button>
                <figcaption>${credit}</figcaption>
            </figure>

            <section class="museum-entry-copy">
                <div class="museum-entry-ornament">◆</div>
                <p class="museum-entry-description">
                    ${wikiInfo?.description || 'No encyclopedia summary is available for this genus yet.'}
                </p>

                <h3>Classification</h3>
                <div class="museum-entry-lineage">${lineage || '<span>Dinosauria</span>'}</div>

                ${wikiInfo?.url ? `
                    <a class="museum-entry-read-more" href="${wikiInfo.url}"
                       target="_blank" rel="noopener">
                        <span>Read the full Wikipedia article</span>
                        <i class="ui-icon ui-icon-external" aria-hidden="true"></i>
                    </a>
                ` : ''}
            </section>
        </div>
    `;
}

async function showMuseum() {
    setAppRoute('/museum');
    setHeaderControls('museum');
    const appContent = document.getElementById('app-content');
    
    appContent.innerHTML = `<div class="game-card"><div class="loading">Loading museum...</div></div>`;
    
    try {
        if (!fullDatabase || fullDatabase.length === 0) {
            const catalog = await callGameApi('catalog');
            fullDatabase = catalog.dinosaurs || [];
        }

        museumDiscoveryRecords = await getDiscoveryRecords();
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
                <h2 style="color:var(--color-primary); margin-bottom:20px; text-align:center; font-size:2em; letter-spacing:3px;">
                    MUSEUM
                </h2>
                
                <div class="museum-progress-container">
                    <div style="font-size:1.1em; color:var(--color-secondary); font-weight:600;">
                        UNLOCKED: ${totalUnlocked} / ${totalCount} (${totalPercent}%)
                    </div>
                    <div class="museum-progress-bar">
                        <div class="museum-progress-fill" style="width: ${totalPercent}%;"></div>
                    </div>
                    <div style="font-size:0.85em; color:var(--color-muted); font-style:italic;">
                        Complete challenges or practice games to unlock dinosaurs.
                    </div>
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
            const cardData = `data-museum-level="${dino.dificuldade}" data-museum-name="${escapeChallengeHtml(dino.nome.toLowerCase())}" data-museum-unlocked="${isUnlocked}"`;

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
                            <img class="museum-card-art" id="art-${dino.nome.replace(/\s+/g, '')}" src="dinosaur-footprint-1-svgrepo-com.svg" alt="${dino.nome}" />
                        </div>
                        <div class="museum-card-name">${dino.nome}</div>
                        <div class="museum-card-clade">${lastClade}</div>
                        <div class="museum-card-discovery">
                            <span>${discovery.firstLabel}</span>
                            ${museumDiscoveryRecords[dino.nome.toLowerCase()]?.count > 1
                                ? `<strong>${discovery.countLabel}</strong>`
                                : ''}
                        </div>
                        <div id="source-${dino.nome.replace(/\s+/g, '')}"
                             style="font-size:0.68em; line-height:1.3; margin-top:6px;"></div>
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
            <div id="clade-info"></div>`;
        appContent.innerHTML = html;

        applyMuseumFilters();

        museumDinos.forEach(async dino => {
            if (unlockedSet.has(dino.nome.toLowerCase())) {
                const imgElement = document.getElementById(`art-${dino.nome.replace(/\s+/g, '')}`);
                if (imgElement) {
                    const media = await getCachedDinoMedia(dino.nome);
                    imgElement.src = media?.url || 'dinosaur-footprint-1-svgrepo-com.svg';
                    imgElement.classList.add('loaded');

                    if (media?.source === 'wikimedia' || media?.source === 'dinopedia') {
                        const sourceElement = document.getElementById(`source-${dino.nome.replace(/\s+/g, '')}`);
                        if (sourceElement) {
                            const sourceName = media.source === 'dinopedia' ? 'Dinopedia' : 'Commons';
                            sourceElement.innerHTML = `
                                <a href="${media.file_page}" target="_blank"
                                   onclick="event.stopPropagation()"
                                   style="color:var(--color-muted); text-decoration:none;">
                                    ${media.artist || sourceName} · ${media.license}
                                </a>
                            `;
                        }
                    }
                }
            }
        });

    } catch (err) {
        console.error('Museum Error:', err);
        appContent.innerHTML = `<div class="game-card" style="color:var(--color-danger);">Error loading Museum: ${err.message}</div>`;
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
        const isVisible = matchesLevel && matchesSearch;

        card.hidden = !isVisible;
        if (!isVisible) return;

        visibleCount += 1;
        if (card.dataset.museumUnlocked === 'true') visibleUnlocked += 1;
    });

    const summary = document.getElementById('museum-filter-summary');
    if (summary) {
        const specimenLabel = visibleCount === 1 ? 'specimen' : 'specimens';
        summary.textContent = `Showing ${visibleCount} ${specimenLabel} · ${visibleUnlocked} unlocked`;
    }

    const emptyState = document.getElementById('museum-empty-state');
    if (emptyState) emptyState.hidden = visibleCount !== 0;
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

    appContent.innerHTML = '<div class="game-card loading">Loading private analytics…</div>';

    let data;
    try {
        data = await callGameApi('analytics_dashboard', { days });
    } catch (error) {
        appContent.innerHTML = `<div class="game-card empty-state" style="color:var(--color-danger-light);">Could not load analytics.<br>${escapeChallengeHtml(error.message)}</div>`;
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
        const context = [analyticsLabel(event.mode), analyticsLabel(event.difficulty)].filter(value => value && value !== 'Unknown').join(' · ');
        return `<div class="analytics-activity-row">
            <span>◆</span>
            <div><strong>${escapeChallengeHtml(analyticsLabel(event.type))}</strong>${context ? `<small>${escapeChallengeHtml(context)}</small>` : ''}</div>
            <time>${escapeChallengeHtml(when)}</time>
        </div>`;
    }).join('');

    appContent.innerHTML = `
    <div class="game-card analytics-dashboard">
        <div class="analytics-header">
            <div>
                <div class="friends-kicker">Private Analytics</div>
                <h2>Phylosaur Analytics</h2>
                <p>Aggregated usage only. No emails, usernames, IP addresses or fingerprints are displayed.</p>
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
            ${analyticsMetricCard('Anonymous sessions', summary.anonymousSessions)}
            ${analyticsMetricCard('Friend challenges', summary.challengesCreated, `${summary.challengeJoins} joins`)}
            ${analyticsMetricCard('Museum views', summary.museumViews)}
        </div>

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

        <p class="analytics-generated">Generated ${escapeChallengeHtml(new Date(data.generatedAt).toLocaleString())}</p>
    </div>`;
}