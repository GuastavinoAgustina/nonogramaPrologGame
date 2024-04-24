import React from 'react';

function Square({ value, onClick, i, j }) {
    return (
        <button id={"square"+i+j} className="square" onClick={onClick}>
            {value !== '_' ? value : null}
        </button>
    );
}

export default Square;