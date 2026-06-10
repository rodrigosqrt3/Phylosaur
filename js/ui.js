// ═══════════════════════════════════════════════
// UI AND MODALS
// ═══════════════════════════════════════════════
function setHeaderControls(screen) {
    const controls = document.getElementById('header-controls');
    if (!controls) return;

    const themeBtn = `
      <button class="theme-switch" onclick="toggleTheme()" id="theme-toggle" aria-label="Toggle theme">
        <span class="theme-switch-track">
          <span class="theme-switch-thumb"></span>
        </span>
      </button>
    `;

    const aboutBtn = `<button class="btn-hint btn-header" onclick="showAbout()" style="padding:8px 14px; font-size:12px;">About</button>`;
    
    const statsBtn = currentUser 
      ? `<button class="btn-hint btn-header" onclick="showStatsDashboard()">Stats</button>` 
      : '';

    const logoutBtn = currentUser 
      ? `<button class="btn-hint btn-header" onclick="logout()">${currentUser}</button>` 
      : '';

    const backBtn = `<button class="btn-hint" onclick="showDifficultySelection()" style="padding:8px 14px; font-size:12px;">← Levels</button>`;

const map = {
      'login':        themeBtn,
      'difficulty': themeBtn + aboutBtn + `<button class="btn-hint btn-header" onclick="showMuseum()">Museum</button>` + (currentUser ? statsBtn + logoutBtn : `<button class="btn-hint btn-header" onclick="showLoginModal()">Sign In</button>`),
      'game': themeBtn + aboutBtn + backBtn + (currentUser ? statsBtn : `<button class="btn-hint btn-header" onclick="showLoginModal()">Sign In</button>`),
      'stats':        themeBtn + `<button class="btn-hint btn-header" onclick="showDifficultySelection()">← Back</button>`,
      'museum':       themeBtn + `<button class="btn-hint btn-header" onclick="showDifficultySelection()">← Back</button>`, // <-- ADD THIS LINE
      'about':        themeBtn + `<button class="btn-hint btn-header" onclick="showDifficultySelection()">← Back</button>`,
      'practice-menu':themeBtn + aboutBtn + `<button class="btn-hint btn-header" onclick="showDifficultySelection()">← Back</button>`,
      'practice':     themeBtn + aboutBtn + `<button class="btn-hint btn-header" onclick="showPracticeMode()">← Back</button>`,
    };

    controls.innerHTML = map[screen] || themeBtn;
}

function toggleTheme() {
        currentTheme = currentTheme === 'dark' ? 'light' : 'dark';
        document.body.classList.toggle('light-mode');
        localStorage.setItem('phylosaur-theme', currentTheme);
    }

function showModal(options) {
    return new Promise((resolve) => {
    const overlay = document.createElement('div');
    overlay.className = 'modal-overlay';
    
    const box = document.createElement('div');
    box.className = 'modal-box';
    
    let html = '';
    
    if (options.title) {
        html += `<div class="modal-title">${options.title}</div>`;
    }
    
    if (options.message) {
        html += `<div class="modal-message">${options.message}</div>`;
    }
    
    if (options.info) {
        html += '<div class="modal-info">';
        options.info.forEach(item => {
        html += `
            <div class="modal-info-item">
            <span class="modal-info-label">${item.label}:</span>
            <span>${item.value}</span>
            </div>
        `;
        });
        html += '</div>';
    }
    
    html += '<div class="modal-buttons">';
    
    if (options.buttons) {
        options.buttons.forEach((btn, index) => {
        const btnClass = btn.primary ? 'modal-btn-primary' : 'modal-btn-secondary';
        html += `<button class="modal-btn ${btnClass}" data-result="${btn.value}">${btn.text}</button>`;
        });
    } else {
        html += '<button class="modal-btn modal-btn-primary" data-result="ok">OK</button>';
    }
    
    html += '</div>';
    
    box.innerHTML = html;
    overlay.appendChild(box);
    document.body.appendChild(overlay);
    
    box.querySelectorAll('.modal-btn').forEach(btn => {
        btn.addEventListener('click', () => {
        const result = btn.getAttribute('data-result');
        document.body.removeChild(overlay);
        resolve(result);
        });
    });
    
    overlay.addEventListener('click', (e) => {
        if (e.target === overlay && options.closeOnOverlay !== false) {
        document.body.removeChild(overlay);
        resolve(null);
        }
    });
    });
}

function customAlert(title, message) {
    return showModal({
    title: title,
    message: message,
    buttons: [{ text: 'OK', value: 'ok', primary: true }]
    });
}

function customConfirm(title, message, yesText = 'Yes', noText = 'No') {
    return showModal({
    title: title,
    message: message,
    buttons: [
        { text: yesText, value: true, primary: true },
        { text: noText, value: false, primary: false }
    ]
    });
}

function openImageLightbox(url, name, commonsPage) {
    const overlay = document.createElement('div');
    overlay.style.cssText = `
    position:fixed; top:0; left:0; right:0; bottom:0;
    background:rgba(0,0,0,0.92); z-index:99999;
    display:flex; flex-direction:column;
    align-items:center; justify-content:center;
    cursor:zoom-out; animation:fadeIn 0.2s ease;
    padding:20px;
    `;
    
    overlay.innerHTML = `
    <img src="${url}" alt="${name}"
        style="max-width:90vw; max-height:80vh; border-radius:8px;
                border:2px solid var(--border-subtle); box-shadow:0 8px 40px rgba(0,0,0,0.8);" />
    <div style="margin-top:16px; font-family:Georgia,serif; font-style:italic;
                color:var(--color-accent); font-size:1.1em; letter-spacing:1px;">${name}</div>
    <div style="margin-top:8px; font-size:0.82em; color:var(--border-subtle); letter-spacing:1px;">
        Art by <a href="https://totaldino.com" target="_blank" 
                style="color:var(--color-muted); text-decoration:none;"
                onclick="event.stopPropagation()">TotalDino</a>
        · <a href="${commonsPage}" target="_blank"
            style="color:var(--color-muted); text-decoration:none;"
            onclick="event.stopPropagation()">View on Wikimedia Commons</a>
    </div>
    <div style="margin-top:16px; color:var(--border-subtle); font-size:0.8em; letter-spacing:2px;">
        CLICK ANYWHERE TO CLOSE
    </div>
    `;
    
    overlay.onclick = () => document.body.removeChild(overlay);
    document.body.appendChild(overlay);
}

async function showCladeInfo(cladeName) {
    const infoDiv = document.getElementById('clade-info');
    infoDiv.innerHTML = `
    <div class="clade-info">
        <div class="loading">Retrieving phylogenetic information for ${cladeName}…</div>
    </div>
    `;

    const wikiInfo = await fetchWikipediaInfo(cladeName);
    
    if (!wikiInfo) {
    infoDiv.innerHTML = `
        <div class="clade-info">
        <h3>Clade: ${cladeName}</h3>
        <p style="color:#999;font-style:italic;">No encyclopedia entry found.</p>
        </div>
    `;
    return;
    }

    let html = `<div class="clade-info"><h3>Clade: ${cladeName}</h3><div class="clade-content">`;
    
    if (wikiInfo.image) {
    html += `<img src="${wikiInfo.image}" alt="${wikiInfo.title}" class="clade-image" />`;
    }
    
    html += `
    <div class="clade-text">
        ${wikiInfo.description 
        ? `<p>${wikiInfo.description}</p>` 
        : '<p style="color:#999;font-style:italic;">Description unavailable.</p>'
        }
        <a href="${wikiInfo.url}" target="_blank" class="clade-link">View Encyclopedia Entry</a>
    </div>
    `;
    
    html += `</div></div>`;
    infoDiv.innerHTML = html;
}

async function updateCladeInfo() {
    const infoDiv = document.getElementById('clade-info');
    
    if (guesses.length === 0) {
    infoDiv.innerHTML = '';
    return;
    }

    const bestGuess = guesses.reduce((b, c) => 
    c.proximity.matches > b.proximity.matches ? c : b
    );
    
    const lastCommonClade = bestGuess.proximity.lastCommonClade;
    
    if (!lastCommonClade) {
    infoDiv.innerHTML = '';
    return;
    }

    infoDiv.innerHTML = `
    <div class="clade-info">
        <div class="loading">Retrieving phylogenetic information for ${lastCommonClade}…</div>
    </div>
    `;

    const wikiInfo = await fetchWikipediaInfo(lastCommonClade);
    
    if (!wikiInfo) {
    infoDiv.innerHTML = `
        <div class="clade-info">
        <h3>Most Recent Common Ancestor: ${lastCommonClade}</h3>
        <p style="color:#999;font-style:italic;">No encyclopedia entry found.</p>
        </div>
    `;
    return;
    }

    let html = `
    <div class="clade-info">
        <h3>Most Recent Common Ancestor: ${lastCommonClade}</h3>
        <div class="clade-content">
    `;
    
    if (wikiInfo.image) {
    html += `<img src="${wikiInfo.image}" alt="${wikiInfo.title}" class="clade-image" />`;
    }
    
    html += `
    <div class="clade-text">
        ${wikiInfo.description 
        ? `<p>${wikiInfo.description}</p>` 
        : '<p style="color:#999;font-style:italic;">Description unavailable.</p>'
        }
        <a href="${wikiInfo.url}" target="_blank" class="clade-link">View Encyclopedia Entry</a>
    </div>
    `;
    
    html += `</div>`;

    html += `<div class="phylo-path"><h4>Revealed Phylogenetic Path to Mystery Taxon:</h4>`;
    for (let i = 0; i < bestGuess.proximity.matches; i++) {
    html += `
        <div class="phylo-step">
        <span class="phylo-step-name">${targetDino.linhagem[i]}</span>
        </div>
    `;
    }
    html += `</div></div>`;
    
    infoDiv.innerHTML = html;
}

function updateGuessHistory() {
    const historyDiv = document.getElementById('guess-history');

    if (!historyDiv) return;
    
    if (guesses.length === 0 && hintHistory.length === 0) {
    historyDiv.innerHTML = '';
    return;
    }

    if (guesses.length === 0 && hintHistory.length === 0) {
    historyDiv.innerHTML = '';
    return;
    }
    
    let html = '<div class="guess-history"><h3>Exploration History</h3>';
    
    if (hintHistory.length > 0) {
    hintHistory.slice().reverse().forEach(hint => {
        html += `
        <div class="guess-item guess-item-hint">
            <span class="guess-name">[HINT: ${hint.cladeName}]</span>
            <span class="guess-match">Revealed clade at depth ${hint.depth}</span>
        </div>
        `;
    });
    }
    
    guesses.slice().reverse().forEach(guess => {
    const divInfo = guess.proximity.lastCommonClade 
        ? ` → Last common: ${guess.proximity.lastCommonClade}` 
        : '';
    
    html += `
        <div class="guess-item">
        <span class="guess-name">${guess.dino.nome}${divInfo}</span>
        <span class="guess-match">${guess.proximity.matches}/${targetDino.linhagem.length} nodes (${guess.proximity.percentage}%)</span>
        </div>
    `;
    });
    
    html += '</div>';
    historyDiv.innerHTML = html;
}