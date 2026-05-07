// ═══════════════════════════════════════════════
// AUTOCOMPLETE
// ═══════════════════════════════════════════════
function initializeAutocomplete() {
  const input = document.getElementById('dino-input');
  const sugDiv = document.getElementById('suggestions');
  
  if (!input || !sugDiv) return;

  input.addEventListener('input', function() {
    const text = this.value.toLowerCase().trim();
    
    if (text.length < 2) {
      sugDiv.style.display = 'none';
      return;
    }
    
    const matches = database
      .filter(d => 
        d.nome.toLowerCase().includes(text) && 
        !guessedNames.has(d.nome.toLowerCase())
      )
      .slice(0, 8);
    
    if (!matches.length) {
      sugDiv.style.display = 'none';
      return;
    }
    
    sugDiv.innerHTML = matches
      .map(d => `<div class="suggestion-item" onclick="selectSuggestion('${d.nome}')">${d.nome}</div>`)
      .join('');
    
    sugDiv.style.display = 'block';
  });

  input.addEventListener('keydown', e => {
    const items = sugDiv.querySelectorAll('.suggestion-item');
    const current = sugDiv.querySelector('.suggestion-item.highlighted');
    const currentIndex = Array.from(items).indexOf(current);

    if (e.key === 'ArrowDown') {
      e.preventDefault();
      if (sugDiv.style.display === 'none' || items.length === 0) return;
      if (current) current.classList.remove('highlighted');
      const next = items[currentIndex + 1] || items[0];
      next.classList.add('highlighted');
      input.value = next.textContent.trim();
    } else if (e.key === 'ArrowUp') {
      e.preventDefault();
      if (sugDiv.style.display === 'none' || items.length === 0) return;
      if (current) current.classList.remove('highlighted');
      const prev = items[currentIndex - 1] || items[items.length - 1];
      prev.classList.add('highlighted');
      input.value = prev.textContent.trim();
    } else if (e.key === 'Enter') {
      if (current) {
        input.value = current.textContent.trim();
        sugDiv.style.display = 'none';
        current.classList.remove('highlighted');
      } else {
        makeGuess();
      }
    } else if (e.key === 'Tab') {
      e.preventDefault();
      const first = sugDiv.querySelector('.suggestion-item');
      if (first) {
        input.value = first.textContent.trim();
        sugDiv.style.display = 'none';
      }
    } else if (e.key === 'Escape') {
      sugDiv.style.display = 'none';
      if (current) current.classList.remove('highlighted');
    }
  });
  
  document.addEventListener('click', e => {
    if (e.target !== input) sugDiv.style.display = 'none';
  });
}

function selectSuggestion(name) {
  const input = document.getElementById('dino-input');
  const sugDiv = document.getElementById('suggestions');
  
  if (input && sugDiv) {
    input.value = name;
    sugDiv.style.display = 'none';
    input.focus();
  }
}