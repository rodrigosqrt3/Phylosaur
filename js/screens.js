// ═══════════════════════════════════════════════
// SCREENS AND INTERFACE LOGIC
// ═══════════════════════════════════════════════
async function showDifficultySelection() {
    setHeaderControls('difficulty');
    const appContent = document.getElementById('app-content');
    const completionStatus = await getDailyCompletionStatus();
    
    appContent.innerHTML = `            
        <div class="game-card" style="text-align:center;">
        <h2 style="color:var(--color-primary); margin-bottom:12px; font-size:1.8em;">Daily Challenge</h2>
        <p style="color:var(--color-secondary); margin-bottom:30px; font-size:0.95em; letter-spacing:1px;">
            ${getCurrentDateFormatted()} — Choose a level
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
            <button class="btn-hint" onclick="showHowToPlay()" style="padding:18px 40px; font-size:15px;">
                How to Play
            </button>
            <button class="btn-hint" onclick="showPracticeMode()" style="padding:18px 40px; font-size:15px;">
                Practice Mode
            </button>
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

function showCompletedChallenge(difficulty, result) {
    setHeaderControls('game');
    selectedDifficulty = difficulty;
    const appContent = document.getElementById('app-content');
    
    appContent.innerHTML = `


    <div class="game-card">
        <div class="challenge-completed-banner" style="${result.gaveUp ? 'background:var(--color-danger-bg); border-color:var(--color-danger-dark);' : ''}">
            <h2 style="${result.gaveUp ? 'color:var(--color-danger);' : ''}">${result.gaveUp ? 'You Gave Up' : 'Challenge Completed'}</h2>
            <div class="dino-name">${result.targetDino}</div>
            <div class="attempts">${result.gaveUp ? `After ${result.guessCount} attempts` : `Found in ${result.guessCount} attempts`}</div>
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
        <div class="stat"><div class="stat-value">${stats?.best_score || '—'}</div><div class="stat-label">Best Score</div></div>
        </div>

        ${generateStreakDisplay(streakData)}

        <div style="background:var(--bg-panel); padding:25px; border-radius:8px; margin-bottom:30px; border:2px solid var(--border-subtle);">
        <h3 style="color:var(--color-text-light); margin-bottom:20px; font-size:1.3em; border-bottom:2px solid var(--border-subtle); padding-bottom:12px;">Performance by Level</h3>
        ${generateDifficultyStats(diffStats)}
        </div>

        <div style="background:var(--bg-panel); padding:25px; border-radius:8px; margin-bottom:30px; border:2px solid var(--border-subtle);">
        <h3 style="color:var(--color-text-light); margin-bottom:20px; font-size:1.3em; border-bottom:2px solid var(--border-subtle); padding-bottom:12px;">Achievements</h3>
        ${generateAchievements(unlockedAchievements)}
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

function showAbout() {
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
            — Richard Owen (1842). Report on British Fossil Reptiles, Part II. <em>Report of the British Association 
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
            TotalDino and reusable files identified through Wikimedia Commons or explicitly licensed Dinopedia pages. 
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
            $$m(A,B)=\\max\\left\\{k: L_A[i]=L_B[i]\\;\\text{for every}\\;0\\le i<k\\right\\}$$
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
            of the target path. Hints reduce uncertainty more directly by exposing the next clade, and the waiting rule 
            between hints creates space for the player to use the new information. Dinosaur branches are highly uneven: 
            one clade may contain hundreds of candidates while another contains only a few. Consequently, different 
            guesses can provide very different amounts of information, which is an important part of the strategy.
        </p>
        <p style="margin-bottom:25px;">
            Difficulty stratification has its own statistical basis. Let $V=\\{v_1,v_2,\\ldots,v_n\\}$ be the 
            sorted vector of usable thirty-day pageview totals. Boundaries near the 20th, 40th, 60th, and 80th 
            percentiles divide the observed distribution into five familiarity bands. These are empirical game-design 
            categories, shaped by language, public interest, media attention, and the collection period. Genera without 
            reliable traffic data—including ambiguous homonyms—are handled conservatively.
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
            <strong style="color:var(--color-primary);">1. Guess a genus</strong><br>
            Type any dinosaur name and submit. The tree will reveal how closely related your guess is to the target.
        </div>

        <div style="margin-bottom:12px; padding:12px; background:rgba(61,47,31,0.4); border-radius:4px;">
            <strong style="color:var(--color-primary);">2. Read the tree</strong><br>
            Each guess reveals the deepest clade shared with the mystery dinosaur. The closer on the tree, the warmer you are.
        </div>

        <div style="margin-bottom:12px; padding:12px; background:rgba(61,47,31,0.4); border-radius:4px;">
            <strong style="color:var(--color-primary);">3. Hints</strong><br>
            You have 3 hints per challenge. Each hint reveals the next clade in the target's lineage. You must make 2 guesses between hints.
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
        { text: 'Got It', value: 'ok', primary: true }
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

function generateAchievements(unlockedSet) {
    const allAchievements = [
    { id: 'first_win', name: 'First Win', desc: 'Complete your first challenge' },
    { id: 'perfect_game', name: 'Three Guesses', desc: 'Find the answer in 3 guesses or fewer' },
    { id: 'ten_wins', name: '10 Wins', desc: 'Complete 10 challenges' },
    { id: 'fifty_wins', name: '50 Wins', desc: 'Complete 50 challenges' },
    { id: 'hard_win', name: 'Level IV', desc: 'Complete a Level IV challenge' },
    { id: 'very_hard_win', name: 'Level V', desc: 'Complete a Level V challenge' }
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
let selectedMuseumLevel = 'muito_facil';

let museumFallbackCatalogPromise = null;

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
            firstLabel: 'Discovery date unavailable',
            countLabel: 'Discovered once',
            lastLabel: ''
        };
    }

    const firstDate = formatMuseumDiscoveryDate(record.firstDiscoveredAt);
    const lastDate = formatMuseumDiscoveryDate(record.lastDiscoveredAt);
    const firstLabel = record.firstDateUnknown
        ? 'Discovered before date tracking'
        : firstDate
            ? `First discovered ${firstDate}`
            : 'Discovery date unavailable';

    return {
        firstLabel,
        countLabel: record.count === 1
            ? 'Discovered once'
            : `Discovered ${record.count} times`,
        lastLabel: record.count > 1 && lastDate
            ? `Most recently ${lastDate}`
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
        Art by <a href="https://totaldino.com" target="_blank" rel="noopener">TotalDino</a>
        · <a href="${commonsPage}" target="_blank" rel="noopener">Wikimedia Commons</a>
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
    closeMuseumEntry();

    const dino = fullDatabase.find(item => item.nome === name);
    if (!dino) return;

    const overlay = document.createElement('div');
    overlay.id = 'museum-entry-overlay';
    overlay.className = 'museum-entry-overlay';
    overlay.innerHTML = `
        <article class="museum-entry-dialog" role="dialog" aria-modal="true" aria-label="${name}">
            <button class="museum-entry-close" type="button" onclick="closeMuseumEntry()" aria-label="Close">×</button>
            <div class="museum-entry-loading">Opening ${name}…</div>
        </article>
    `;

    overlay.addEventListener('click', event => {
        if (event.target === overlay) closeMuseumEntry();
    });

    document.body.appendChild(overlay);
    document.body.style.overflow = 'hidden';

    museumEntryEscapeHandler = event => {
        if (event.key !== 'Escape') return;
        const viewer = document.getElementById('museum-image-viewer');
        if (viewer) viewer.remove();
        else closeMuseumEntry();
    };
    document.addEventListener('keydown', museumEntryEscapeHandler);

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
        <button class="museum-entry-close" type="button" onclick="closeMuseumEntry()" aria-label="Close">×</button>

        <header class="museum-entry-header">
            <div class="museum-entry-kicker">Museum record</div>
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
                        Read the full Wikipedia article ↗
                    </a>
                ` : ''}
            </section>
        </div>
    `;
}

async function showMuseum() {
    setHeaderControls('museum');
    const appContent = document.getElementById('app-content');
    
    appContent.innerHTML = `<div class="game-card"><div class="loading">Loading museum...</div></div>`;
    
    try {
        if (!fullDatabase || fullDatabase.length === 0) {
            const res = await fetch('phylosaur_db.json');
            fullDatabase = await res.json();
        }

        museumDiscoveryRecords = await getDiscoveryRecords();
        const unlockedList = Object.values(museumDiscoveryRecords)
            .map(record => record.name);
        const unlockedSet = new Set(unlockedList.map(name => name.toLowerCase()));

        const levelDinos = fullDatabase.filter(d => d.dificuldade === selectedMuseumLevel);
        const levelUnlockedCount = levelDinos.filter(d => unlockedSet.has(d.nome.toLowerCase())).length;
        
        const totalCount = fullDatabase.length;
        const totalUnlocked = unlockedList.length;
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

                <!-- Level Tabs -->
                <div class="tab-row museum-tabs" style="margin-bottom: 24px;">
                    <button class="tab-btn ${selectedMuseumLevel === 'muito_facil' ? 'active' : ''}" onclick="switchMuseumLevel('muito_facil')">Level I</button>
                    <button class="tab-btn ${selectedMuseumLevel === 'facil' ? 'active' : ''}" onclick="switchMuseumLevel('facil')">Level II</button>
                    <button class="tab-btn ${selectedMuseumLevel === 'normal' ? 'active' : ''}" onclick="switchMuseumLevel('normal')">Level III</button>
                    <button class="tab-btn ${selectedMuseumLevel === 'dificil' ? 'active' : ''}" onclick="switchMuseumLevel('dificil')">Level IV</button>
                    <button class="tab-btn ${selectedMuseumLevel === 'muito_dificil' ? 'active' : ''}" onclick="switchMuseumLevel('muito_dificil')">Level V</button>
                </div>

                <div style="font-size:1em; color:var(--color-secondary); text-align:center; margin-bottom:15px; font-weight:600;">
                    Unlocked in this level: ${levelUnlockedCount} / ${levelDinos.length}
                </div>

                <div class="museum-grid">
        `;

        levelDinos.forEach(dino => {
            const isUnlocked = unlockedSet.has(dino.nome.toLowerCase());
            const lastClade = dino.linhagem[dino.linhagem.length - 1] || 'Dinosauria';

            if (isUnlocked) {
                const discovery = getMuseumDiscoverySummary(
                    museumDiscoveryRecords[dino.nome.toLowerCase()]
                );
                html += `
                    <div class="museum-card unlocked" onclick="showMuseumEntry('${dino.nome}')" style="cursor:pointer;">
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
                    <div class="museum-card locked">
                        <div class="museum-card-art-container">
                            <span class="museum-card-lock-icon">🔒</span>
                        </div>
                        <div class="museum-card-name">???</div>
                        <div class="museum-card-clade">Locked</div>
                    </div>
                `;
            }
        });

        html += `</div></div><div id="clade-info"></div>`;
        appContent.innerHTML = html;

        levelDinos.forEach(async dino => {
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
    showMuseum();
}