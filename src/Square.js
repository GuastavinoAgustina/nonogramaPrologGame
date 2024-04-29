import React from 'react';


function Square({ value, onClick }) {
    return (
        <div
            className={`square ${value === '#' ? 'relleno_negro' : 'relleno_blanco'}`}
            onClick={onClick}
        >
            {value}
        </div>
    );
}

export default Square;