import React from 'react';
import { colorToCss } from './Game';

function Square({ value, onClick, i, j }) {
    return (
        <button id={"square"+i+j} className="square" onClick={onClick}  style={{backgroundColor: colorToCss(value)}}>
            {value !== '_' ? value : null}
        </button>
    );
}

export default Square;