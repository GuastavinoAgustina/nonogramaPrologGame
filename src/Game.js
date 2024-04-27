import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';

let pengine;
export function colorToCss(color) {
  switch (color) {
    case "#": return "black";
    case "_": return "white";
    case "X": return "white";
  }
  return color;
}
function Game() {

  // State
  const [grid, setGrid] = useState(null);
  const [rowsClues, setRowsClues] = useState(null);
  const [colsClues, setColsClues] = useState(null);
  const [waiting, setWaiting] = useState(false);
  const [rowsSat, setRowsSat] = useState(false);
  const [colsSat, setColsSat] = useState(false);
  
  useEffect(() => {
    // Creation of the pengine server instance.    
    // This is executed just once, after the first render.    
    // The callback will run when the server is ready, and it stores the pengine instance in the pengine variable. 
    PengineClient.init(handleServerReady);
  }, []);

  function handleServerReady(instance) {
    pengine = instance;
    const queryS = 'init(RowClues, ColumClues, Grid)';
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['Grid']);
        setRowsClues(response['RowClues']);
        setColsClues(response['ColumClues']);
      }
    });
  }

  function handleClick(i, j, checked) {
    // No action on click if we are waiting.
    if (waiting) {
      return;
    }
    // Build Prolog query to make a move and get the new satisfacion status of the relevant clues.    
    const squaresS = JSON.stringify(grid).replaceAll('"_"', '_'); // Remove quotes for variables. squares = [["X",_,_,_,_],["X",_,"X",_,_],["X",_,_,_,_],["#","#","#",_,_],[_,_,"#","#","#"]]
    let content; // Content to put in the clicked square.
    if (checked){
      content = '#';
    }
    else{
      content = 'X';
    }
     // Content to put in the clicked square.
    const rowsCluesS = JSON.stringify(rowsClues);
    const colsCluesS = JSON.stringify(colsClues);
    
    const queryS = `put("${content}", [${i},${j}], ${rowsCluesS}, ${colsCluesS}, ${squaresS}, ResGrid, RowSat, ColSat)`; // queryS = put("#",[0,1],[], [],[["X",_,_,_,_],["X",_,"X",_,_],["X",_,_,_,_],["#","#","#",_,_],[_,_,"#","#","#"]], GrillaRes, FilaSat, ColSat)
    setWaiting(true);
    
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['ResGrid']);
        setRowsSat(response['RowSat']);
        setColsSat(response['ColSat']);
        console.log(grid);
        console.log("filaSat:"+ setGrid(response['ResGrid']) );
        console.log("columnaSat:"+colsSat);

        console.log("Coordenadas: [f:"+i+",c:"+j+"]");
   
        if(response['RowSat']===1){
          activeClue("row"+i)
          activeClue("row"+i)
          console.log("una fila se satifacio: " + i)
        }
        if(response['RowSat']===0){
          inactiveClue("row"+i)
          console.log("una fila se desactivo: " + i)
        }
    
        if(response['ColSat']===1){
          activeClue("col"+j)
          console.log("una columna se satifacio: " + j)
        }
        if(response['ColSat']===0){
          inactiveClue("col"+j)
          console.log("una columna se desactivo: " + j)
        }
      }      
      setWaiting(false);
    });

  }

  function activeClue(id_name) {
    var element = document.getElementById(id_name);
    element.classList.add("activeClue");
  }
  function inactiveClue(id_name) {
    var element = document.getElementById(id_name);
    element.classList.remove("activeClue");
  }

  // funcion que permite saber si el toggle esta en X (true) o en # (false) 
  const [checked, setChecked] = React.useState(true);
  const handleChange = () => {
    setChecked(!checked);
  };

  


  if (!grid) {
    return null;
  }
  const statusText = '';
 
  return (
    <div className="game">
      <Board
        grid={grid}
        rowsClues={rowsClues}
        colsClues={colsClues}        
        onClick={(i, j) => handleClick(i, j, checked)}
      />
      <div className="game-info">
        {statusText}
        <div class="settings">
            <label class="switch">
              <input
                type="checkbox"
                checked={checked}
                value={checked}
                onChange={handleChange}
              />
              <span class="slider round"></span>
            </label>
            <p>Is "My Value" checked?</p>
            <div id="myDIV">
              This is a DIV element.
            </div>
        </div>
      </div>
    </div>
  );
}

export default Game;