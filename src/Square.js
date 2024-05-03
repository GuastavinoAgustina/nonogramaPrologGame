import React from 'react';

function Square({ value, onClick, disabled }) {
  let className = "square";
  if (value === '_') {
    className += " relleno_blanco"; 
  } else if (value === '#') {
    className += " relleno_negro"; 
  }

  return (
    <button className={className} onClick={onClick} disabled={disabled}>
      {value !== '_' && value} {/* Si el valor no es '_', muestra el valor */}
    </button>
  );
}

export default Square;