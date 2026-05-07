// ═══════════════════════════════════════════════
// SCREENS AND INTERFACE LOGIC
// ═══════════════════════════════════════════════
async function showDifficultySelection() {
    setHeaderControls('difficulty');
    const appContent = document.getElementById('app-content');
    const completionStatus = await getDailyCompletionStatus();
    
    appContent.innerHTML = `            
        <div class="game-card" style="text-align:center;">
        <h2 style="color:#d4b87e; margin-bottom:12px; font-size:1.8em;">Daily Challenge</h2>
        <p style="color:#a68a5a; margin-bottom:30px; font-size:0.95em; letter-spacing:1px;">
            ${getCurrentDateFormatted()} — Select Classification Level
        </p>
        <p style="color:#6b5340; font-size:0.82em; font-style:italic; letter-spacing:1px; margin-top:8px;">
        Next daily challenge in <span id="countdown-timer" style="color:#a68a5a; font-weight:600;">--:--:--</span>
        </p>

        <div style="display:grid; grid-template-columns:repeat(auto-fit,minmax(220px,1fr)); gap:20px; margin:30px 0;">
            ${generateDifficultyButton('muito_facil', 'LEVEL I', 'I', '', completionStatus.muito_facil)}
            ${generateDifficultyButton('facil', 'LEVEL II', 'II', '', completionStatus.facil)}
            ${generateDifficultyButton('normal', 'LEVEL III', 'III', '', completionStatus.normal)}
            ${generateDifficultyButton('dificil', 'LEVEL IV', 'IV', '', completionStatus.dificil)}
            ${generateDifficultyButton('muito_dificil', 'LEVEL V', 'V', '', completionStatus.muito_dificil)}
        </div>

        <div style="margin-top:30px; text-align:center;">
            <div style="display:flex; gap:15px; justify-content:center; flex-wrap:wrap;">
            <button class="btn-hint" onclick="showHowToPlay()" style="padding:18px 40px; font-size:15px;">
                How to Play
            </button>
            <button class="btn-hint" onclick="showPracticeMode()" style="padding:18px 40px; font-size:15px;">
                Practice Mode
            </button>
            </div>
        </div>
        
        <div class="difficulty-info-box">
        <div class="difficulty-info-text">
            Each difficulty level presents a unique specimen for today's date. All players worldwide receive identical classification challenges. Progress is tracked independently per difficulty level.
        </div>
        </div>
    `;
    startCountdown();
}

function showPracticeMode() {
    setHeaderControls('practice-menu');
    const appContent = document.getElementById('app-content');
    
    appContent.innerHTML = `
    <div class="game-card" style="text-align:center;">
        <h2 style="color:#d4b87e; margin-bottom:12px; font-size:1.8em;">Practice Mode</h2>
        <p style="color:#a68a5a; margin-bottom:30px; font-size:0.95em; letter-spacing:1px;">
        Train your phylogenetic skills — Select classification level
        </p>

        <div style="display:grid; grid-template-columns:repeat(auto-fit,minmax(200px,1fr)); gap:20px; margin:30px 0;">
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
            style="padding:30px; font-size:1.2em;">
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
        ? '<div style="position:absolute; top:10px; right:10px; width:24px; height:24px; background:#5a9c6b; border-radius:50%; display:flex; align-items:center; justify-content:center; font-size:14px; color:#fff; font-weight:bold;">✓</div>'
        : '';

    const borderClass = completed ? 'difficulty-completed' : '';

    return `
        <button class="difficulty-btn difficulty-${DIFFICULTY_MAP[difficulty]} ${borderClass}" 
                onclick="startDailyChallenge('${difficulty}')" 
                style="padding:30px; font-size:1.2em; position:relative;">
        ${statusIndicator}
        <div style="font-weight:bold; margin-bottom:10px; font-size:1.3em; letter-spacing:3px;">
            ${name}
        </div>
        <div style="margin:8px 0;">
            ${tiers}
        </div>
        ${description ? `<div style="font-size:0.75em; color:#8b7355; font-style:italic;">${description}</div>` : ''}
        </button>
    `;
}

function showCompletedChallenge(difficulty, result) {
    setHeaderControls('game');
    selectedDifficulty = difficulty;
    const appContent = document.getElementById('app-content');
    
    appContent.innerHTML = `


    <div class="game-card">
        <div class="challenge-completed-banner" style="${result.gaveUp ? 'background:rgba(139,58,42,0.2); border-color:#8b3a2a;' : ''}">
            <h2 style="${result.gaveUp ? 'color:#c06040;' : ''}">${result.gaveUp ? 'You Gave Up' : 'Challenge Completed'}</h2>
            <div class="dino-name">${result.targetDino}</div>
            <div class="attempts">${result.gaveUp ? `After ${result.guessCount} attempts` : `Classified in ${result.guessCount} attempts`}</div>
        </div>
        </div>

        <div id="tree-container">
        <div id="tree-scroll-wrapper">
            <div class="loading">Loading phylogenetic tree...</div>
        </div>
        </div>

        <div id="clade-info"></div>
    </div>
    `;
    loadCompletedChallengeTree(difficulty, result);
}

async function showStatsDashboard() {
    setHeaderControls('stats');
    if (!currentUser) {
    alert('Login to view statistics');
    return;
    }

    const { data: stats } = await sb.from('statistics')
    .select('*')
    .eq('user_id', currentUserId)
    .single();

    const { data: diffStats } = await sb.from('difficulty_stats')
    .select('*')
    .eq('user_id', currentUserId);

    const { data: recentGames } = await sb.from('daily_results')
    .select('*')
    .eq('user_id', currentUserId)
    .order('created_at', { ascending: false })
    .limit(10);

    const { data: achievements } = await sb.from('achievements')
    .select('achievement_id')
    .eq('user_id', currentUserId);

    const gamesPlayed = stats?.games_played || 0;
    const gamesWon = stats?.games_won || 0;
    const winRate = gamesPlayed > 0 ? Math.round((gamesWon / gamesPlayed) * 100) : 0;
    const streakData = { current: stats?.current_streak || 0, best: stats?.best_streak || 0, lastPlayed: stats?.last_played };
    const unlockedAchievements = new Set(achievements ? achievements.map(a => a.achievement_id) : []);

    const appContent = document.getElementById('app-content');

    appContent.innerHTML = `


    <div class="game-card">
        <h2 style="color:#d4b87e; margin-bottom:20px; text-align:center; font-size:2em; letter-spacing:3px;">
        CLASSIFICATION RECORDS
        </h2>

        <div style="text-align:center; margin-bottom:30px; color:#a68a5a; font-size:1.1em; letter-spacing:2px;">
        Researcher ID: <span style="color:#c9a96e; font-weight:600;">${currentUser}</span>
        </div>

        <div class="stats" style="grid-template-columns: repeat(2, 1fr); margin-bottom:40px;">
        <div class="stat"><div class="stat-value">${gamesPlayed}</div><div class="stat-label">Total Classifications</div></div>
        <div class="stat"><div class="stat-value">${gamesWon}</div><div class="stat-label">Successful IDs</div></div>
        <div class="stat"><div class="stat-value">${winRate}%</div><div class="stat-label">Success Rate</div></div>
        <div class="stat"><div class="stat-value">${stats?.best_score || '—'}</div><div class="stat-label">Best Score</div></div>
        </div>

        ${generateStreakDisplay(streakData)}

        <div style="background:rgba(61,47,31,0.5); padding:25px; border-radius:8px; margin-bottom:30px; border:2px solid #5d4a36;">
        <h3 style="color:#f4e4c1; margin-bottom:20px; font-size:1.3em; border-bottom:2px solid #5d4a36; padding-bottom:12px;">Classification Level Performance</h3>
        ${generateDifficultyStats(diffStats)}
        </div>

        <div style="background:rgba(61,47,31,0.5); padding:25px; border-radius:8px; margin-bottom:30px; border:2px solid #5d4a36;">
        <h3 style="color:#f4e4c1; margin-bottom:20px; font-size:1.3em; border-bottom:2px solid #5d4a36; padding-bottom:12px;">Research Achievements</h3>
        ${generateAchievements(unlockedAchievements)}
        </div>

        <div style="background:rgba(61,47,31,0.5); padding:25px; border-radius:8px; border:2px solid #5d4a36;">
        <h3 style="color:#f4e4c1; margin-bottom:20px; font-size:1.3em; border-bottom:2px solid #5d4a36; padding-bottom:12px;">Recent Classifications</h3>
        ${generateRecentGames(recentGames)}
        </div>
    </div>
    `;
}

function generateStreakDisplay(streakData) {
    if (!currentUser) return '';

    if (!streakData || streakData.current === 0) {
    return `
        <div style="text-align:center; padding:30px; background:rgba(61,47,31,0.3); border-radius:8px; margin-bottom:30px; border:2px solid #5d4a36;">
        <div style="font-size:1.5em; color:#8b7355; margin-bottom:10px;">◆ Start Your Streak!</div>
        <div style="color:#a68a5a; font-size:0.95em;">Complete a challenge every day to build your research streak</div>
        </div>
    `;
    }

    return `
    <div style="text-align:center; padding:30px; background:rgba(255,149,0,0.1); border-radius:8px; margin-bottom:30px; border:2px solid #ff9500;">
        <div style="font-size:2.5em; color:#ff9500; margin-bottom:15px;">◆ ${streakData.current}</div>
        <div style="font-size:1.2em; color:#d4b87e; margin-bottom:8px; font-weight:600; letter-spacing:1px;">CURRENT STREAK</div>
        <div style="color:#a68a5a; font-size:0.95em;">Best: ${streakData.best} days</div>
        <div style="margin-top:20px; color:#8b7355; font-size:0.85em; font-style:italic;">Last played: ${streakData.lastPlayed || 'Never'}</div>
    </div>
    `;
}

function showAbout() {
    setHeaderControls('about');
    const appContent = document.getElementById('app-content');
    
    appContent.innerHTML = `

    
    <div class="game-card" style="max-width: 900px; margin: 0 auto;">
        <h2 style="color:#d4b87e; margin-bottom:30px; text-align:center; font-size:2em; letter-spacing:3px;">
        About Phylosaur
        </h2>
        
        <div style="color:#f8f6f3; line-height:2; font-size:1.05em; text-align:justify;">

        <p style="margin-bottom:25px; font-style:italic; color:#a68a5a; font-size:0.95em;">
        The name Phylosaur draws from two roots: <em>phylo</em>, evoking both phylogenetics 
        and the Greek φιλέω, to love, and <em>saur</em>, from σαῦρος, the lizard suffix 
        ubiquitous in dinosaur nomenclature. A love of dinosaurs, systematically arranged, 
        even if dinosaurs were never truly lizards.
        </p>
        
        <p style="margin-bottom:25px;">
            Phylosaur is an interactive educational tool designed to familiarize users with the principles 
            of phylogenetic systematics through the lens of dinosaurian taxonomy. The game employs a 
            progressive tree-building mechanism wherein players deduce the systematic position of mystery 
            taxa by analyzing shared derived characteristics (synapomorphies) and identifying most recent 
            common ancestors within the cladistic framework.
        </p>

        <h3 style="color:#c9a96e; margin:35px 0 20px; font-size:1.4em; font-weight:400; letter-spacing:2px; border-bottom:2px solid #5d4a36; padding-bottom:12px;">
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

        <p style="margin-bottom:25px; padding:20px; background:rgba(61,47,31,0.3); border-left:4px solid #8b7355; font-style:italic;">
        "The combination of such characters... will, it is presumed, be deemed sufficient ground for establishing 
        a distinct tribe or sub-order of Saurian Reptiles, for which I would propose the name of <em>Dinosauria</em>."
        <span style="display:block; margin-top:10px; font-style:normal; font-size:0.9em; color:#a68a5a;">
            — Richard Owen (1842). Report on British Fossil Reptiles, Part II. <em>Report of the British Association 
            for the Advancement of Science</em>, 11: 60–204.
        </span>
        </p>

        <h3 style="color:#c9a96e; margin:35px 0 20px; font-size:1.4em; font-weight:400; letter-spacing:2px; border-bottom:2px solid #5d4a36; padding-bottom:12px;">
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
        or systematic resolution. Daily challenges ensure that all participants worldwide confront identical 
        classification problems.
        </p>

        <h3 style="color:#c9a96e; margin:35px 0 20px; font-size:1.4em; font-weight:400; letter-spacing:2px; border-bottom:2px solid #5d4a36; padding-bottom:12px;">
            Data Sources and Taxonomic Authority
        </h3>

        <p style="margin-bottom:25px;">
        Taxonomic occurrence data and genus-level diversity are sourced from the Paleobiology Database 
        (paleobiodb.org), an open-access repository of fossil taxa maintained by the international 
        paleontological community. The database is filtered to include only extinct, valid genera outside 
        crown Aves, removing modern birds and nomenclaturally problematic taxa. Phylogenetic lineages for 
        each genus are retrieved from Wikispecies (species.wikimedia.org), the free species directory, 
        via recursive parsing of its taxonomic template chain. Users should recognize that phylogenetic 
        hypotheses remain subject to revision as new fossil discoveries and analytical methods refine our 
        understanding of dinosaurian relationships. Certain taxa exhibiting nomenclatural ambiguities, 
        such as homonyms with non-paleontological entities, have been manually reviewed and remapped 
        to maintain database integrity.
        </p>

        <p style="margin-bottom:25px;">
        Difficulty classification methodology employs Wikipedia pageview metrics retrieved via the Wikimedia 
        REST API, aggregating thirty-day access statistics for each taxon. Genera are stratified into quintiles 
        based on these pageview counts, with the most frequently accessed taxa assigned to lower difficulty 
        tiers and obscure genera with minimal page traffic assigned to higher difficulty levels. This approach 
        acknowledges that taxonomic familiarity, operationalized here as public digital engagement—constitutes 
        a distinct dimension from phylogenetic complexity, fossil completeness, or systematic significance. 
        Consequently, scientifically important but publicly unfamiliar taxa may appear at higher difficulty 
        levels, while fragmentary or taxonomically problematic genera with substantial popular culture presence 
        may occupy lower tiers.
        </p>

        <p style="margin-bottom:25px;">
            Supplementary information regarding clade diagnoses and evolutionary context is retrieved from 
            Wikipedia's public API. While this resource provides accessible overviews suitable for educational 
            contexts, users are encouraged to consult primary literature (including original species descriptions, 
            phylogenetic analyses published in journals such as <em>Nature</em>, <em>Science</em>, <em>Journal 
            of Vertebrate Paleontology</em>, and <em>Cladistics</em>) for authoritative systematic information.
        </p>

        <h3 style="color:#c9a96e; margin:35px 0 20px; font-size:1.4em; font-weight:400; letter-spacing:2px; border-bottom:2px solid #5d4a36; padding-bottom:12px;">
            Mathematical Foundations
        </h3>
        <p style="margin-bottom:25px;">
            The proximity measure computed by Phylosaur has a precise mathematical interpretation. 
            Given two taxa $A$ and $B$ with lineages $L_A$ and $L_B$ represented as 
            ordered sequences of clades from root to tip, the game computes the length of their 
            longest common prefix , equivalently, the depth of their most recent common ancestor 
            (MRCA). The phylogenetic distance between any two taxa is then formally defined as:
        </p>
        <p style="margin-bottom:25px; padding:20px; background:rgba(61,47,31,0.3); border-radius:4px; text-align:center;">
            $$d(A, B) = \\text{depth}(A) + \\text{depth}(B) - 2 \\cdot \\text{depth}(\\text{MRCA}(A, B))$$
        </p>
        <p style="margin-bottom:25px;">
            This quantity defines a tree metric, it is symmetric, non-negative, and satisfies 
            the triangle inequality $d(A,C) \\leq d(A,B) + d(B,C)$ for all taxa $A$, $B$, $C$. 
            Each guess therefore partitions the remaining hypothesis space: a guess sharing depth 
            $k$ with the target eliminates all taxa residing outside the clade diagnosed at that 
            depth, progressively constraining the search space in a manner analogous to binary 
            search on a sorted structure, though operating on a hierarchical tree topology rather 
            than a linear sequence.
        </p>
        <p style="margin-bottom:25px;">
            From an information-theoretic perspective, each guess constitutes a noisy query 
            revealing the depth of the MRCA between the queried taxon and the hidden target. 
            If we model the target as a uniformly distributed random variable over the 
            difficulty-stratified taxon set of size $n$, the expected number of guesses required 
            for identification scales with the entropy $H = \\log_2(n)$ of the prior 
            distribution. Hints function as direct entropy-reducing revelations, each disclosing 
            one additional node along the target lineage and thereby collapsing the hypothesis 
            space to the subtree rooted at the revealed clade. The constraint requiring two 
            intervening guesses between consecutive hints is designed to preserve the inferential 
            challenge by ensuring players actively engage with the phylogenetic structure rather 
            than exhausting hint allocations sequentially.
        </p>
        <p style="margin-bottom:25px;">
            Difficulty stratification employs a non-parametric quintile decomposition of the 
            Wikipedia pageview distribution. Formally, let $V = \\{v_1, v_2, \\ldots, v_m\\}$ 
            denote the sorted vector of thirty-day pageview counts across all $m$ taxa with 
            positive pageview values. The difficulty boundaries are defined at the 20th, 40th, 
            60th, and 80th percentiles of $V$, partitioning taxa into five equiprobable strata 
            under the empirical pageview distribution. Taxa with zero pageviews, those absent 
            from Wikipedia or generating no recorded traffic, are assigned to the highest 
            difficulty tier by default, as are genera whose names constitute homonyms with 
            non-paleontological entities, for which pageview counts would otherwise reflect 
            unrelated subject matter.
        </p>

        <h3 style="color:#c9a96e; margin:35px 0 20px; font-size:1.4em; font-weight:400; letter-spacing:2px; border-bottom:2px solid #5d4a36; padding-bottom:12px;">
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

        <h3 style="color:#c9a96e; margin:35px 0 20px; font-size:1.4em; font-weight:400; letter-spacing:2px; border-bottom:2px solid #5d4a36; padding-bottom:12px;">
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

        <p style="margin-bottom:35px;">
            Phylosaur is provided as an educational resource without commercial intent. Users are encouraged to 
            provide feedback regarding taxonomic inaccuracies, interface improvements, or pedagogical suggestions 
            that might enhance the tool's effectiveness as an instructional aid. While this application aims for 
            scientific rigor, it should not be cited as a primary source in formal academic contexts; instead, 
            users should trace information to original systematic publications through the references provided in 
            supplementary materials.
        </p>

        </div>

        <p style="margin-bottom:35px; padding:20px; background:rgba(61,47,31,0.3); border-left:4px solid #8b7355; border-radius:2px;">
        Phylosaur is currently under active development. Taxonomic inaccuracies, interface issues, 
        missing genera, or any other problems are expected and your feedback is genuinely invaluable. 
        If you encounter anything worth reporting, however small, please write to 
        <a href="mailto:rodrigo03.villa@gmail.com" style="color:#c9a96e; text-decoration:none; border-bottom:1px solid #8b7355;">rodrigo03.villa@gmail.com</a>. 
        Every message is read and deeply appreciated.
        </p>

        <button class="btn-new-game" onclick="showDifficultySelection()" style="margin-top:30px;">
        Return to Challenge Selection
        </button>
    </div>
    `;
    setTimeout(() => {
    if (window.renderMathInElement) {
        renderMathInElement(document.querySelector('.game-card'), {
        delimiters: [
            { left: '$$', right: '$$', display: true },
            { left: '$', right: '$', display: false }
        ],
        throwOnError: false
        });
    }
    }, 50);
}

function showHowToPlay() {
    showModal({
    title: 'How to Play',
    message: `
        <div style="text-align:left; line-height:2;">
        <p style="margin-bottom:16px;">A mystery dinosaur is hidden each day. Your goal is to identify it through phylogenetic reasoning.</p>
        
        <div style="margin-bottom:12px; padding:12px; background:rgba(61,47,31,0.4); border-radius:4px;">
            <strong style="color:#d4b87e;">1. Guess a genus</strong><br>
            Type any dinosaur name and submit. The tree will reveal how closely related your guess is to the target.
        </div>

        <div style="margin-bottom:12px; padding:12px; background:rgba(61,47,31,0.4); border-radius:4px;">
            <strong style="color:#d4b87e;">2. Read the tree</strong><br>
            Each guess reveals the deepest clade shared with the mystery dinosaur. The closer on the tree, the warmer you are.
        </div>

        <div style="margin-bottom:12px; padding:12px; background:rgba(61,47,31,0.4); border-radius:4px;">
            <strong style="color:#d4b87e;">3. Use hints wisely</strong><br>
            You have 3 hints per challenge. Each hint reveals the next clade in the target's lineage. You must make 2 guesses between hints.
        </div>

        <div style="margin-bottom:12px; padding:12px; background:rgba(61,47,31,0.4); border-radius:4px;">
            <strong style="color:#d4b87e;">4. Five difficulty levels</strong><br>
            Level I features well-known genera. Level V features obscure taxa requiring broad taxonomic knowledge.
        </div>

        <div style="padding:12px; background:rgba(61,47,31,0.4); border-radius:4px;">
            <strong style="color:#d4b87e;">Tip</strong><br>
            Click any node on the tree to read about that clade. New here? Start with Level I.
            <br><br>
            <span style="color:#8b7355; font-style:italic; font-size:0.9em;">
            The game currently has no guess limit while I evaluate what feels most balanced. 
            This may change in future updates.
            </span>
        </div>
        </div>
    `,
    buttons: [
        { text: 'Start Classifying', value: 'ok', primary: true }
    ],
    closeOnOverlay: true
    });
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
    return '<p class="empty-stats">No classifications completed yet. Begin your research!</p>';
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
            <span class="diff-avg">Avg: ${Math.round(stat.avg_guesses)} guesses</span>
        </div>
        </div>
    `;
    });
    return html;
}

function generateAchievements(unlockedSet) {
    const allAchievements = [
    { id: 'first_win', name: 'First Discovery', desc: 'Complete your first classification' },
    { id: 'perfect_game', name: 'Phylogenetic Prodigy', desc: 'Classify species in 3 guesses or fewer' },
    { id: 'ten_wins', name: 'Taxonomist', desc: 'Successfully classify 10 species' },
    { id: 'fifty_wins', name: 'Paleontologist', desc: 'Successfully classify 50 species' },
    { id: 'hard_win', name: 'Specialist', desc: 'Complete Level IV classification' },
    { id: 'very_hard_win', name: 'Master Classifier', desc: 'Complete Level V classification' }
    ];

    let html = '<div style="display:grid; grid-template-columns:repeat(auto-fill, minmax(220px, 1fr)); gap:15px;">';
    allAchievements.forEach(ach => {
    const unlocked = unlockedSet && unlockedSet.has(ach.id);
    html += `
        <div class="achievement-card ${unlocked ? 'achievement-unlocked' : 'achievement-locked'}">
        <div class="achievement-title">${unlocked ? '◆' : '◇'} ${ach.name}</div>
        <div class="achievement-desc">${ach.desc}</div>
        </div>
    `;
    });
    html += '</div>';
    return html;
}

function generateRecentGames(recentGames) {
    if (!recentGames || recentGames.length === 0) {
    return '<p style="color:#8b7355; font-style:italic; padding:20px; text-align:center;">No recent classifications. Begin your research!</p>';
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
        ? '<span style="color:#5d4a36; font-style:italic;">[ classified today — hidden ]</span>' 
        : `<span style="color:#f8f6f3; font-style:italic;">${game.target_dino}</span>`;

    html += `
        <div style="display:flex; justify-content:space-between; align-items:center; padding:15px; margin:8px 0; background:rgba(42,31,22,0.4); border-left:4px solid ${game.won ? '#5a9c6b' : '#8b6f47'}; border-radius:4px;">
        <div style="flex:1;">
            <div style="display:flex; align-items:center; margin-bottom:5px;">
            <span style="color:${game.won ? '#c8e6c9' : '#d4a574'}; font-weight:700; margin-right:12px; font-size:1.1em;">${game.won ? '✓' : '…'}</span>
            ${dinoDisplay}
            </div>
        </div>
        <div style="text-align:right;">
            <div style="color:#c9a96e; font-size:1em; font-weight:600; margin-bottom:3px;">${game.guess_count} guesses</div>
            <div style="color:#8b7355; font-size:0.8em;">${diffNames[game.difficulty]} • ${date}</div>
        </div>
        </div>
    `;
    });
    return html;
}