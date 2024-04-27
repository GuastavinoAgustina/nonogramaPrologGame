import React from 'react';

function Clue({ clue}) {
    return (
        <div class="clue">
            {clue.map((num, i) =>
                <div key={i} id={i}>
                    {num}
                </div>
            )}
        </div>
    );
}

export default Clue;