import React from 'react';

function Square({ value, onClick }) {
  let className = "square";
  if (value === '_') {
    className += " relleno_blanco"; // Si el valor es '_', el cuadrado tiene relleno blanco
  } else if (value === '#') {
    className += " relleno_negro"; // Si el valor es '#', el cuadrado tiene relleno negro
  }

  return (
    <button className={className} onClick={onClick}>
      {value !== '_' && value} {/* Si el valor no es '_', muestra el valor */}
    </button>
  );
}

export default Square;