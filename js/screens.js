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