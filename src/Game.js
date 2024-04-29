import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';

let pengine;

function Game() {

  // State
  const [grid, setGrid] = useState(null);
  const [rowsClues, setRowsClues] = useState(null);
  const [colsClues, setColsClues] = useState(null);
  const [waiting, setWaiting] = useState(false);
  const [rowsCluesSat, setRowsCluesSat] = useState([]);
  const [colsCluesSat, setColsCluesSat] = useState([]);
  
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
        setRowsCluesSat(Array(response['RowClues'].length).fill(0));
        setColsCluesSat(Array(response['ColumClues'].length).fill(0));
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
        rowsCluesSat[i] = response['RowSat'];
        colsCluesSat[j] = response['ColSat'];

        console.log(rowsCluesSat);
        console.log(colsCluesSat);
        console.log(rowsClues);
    
        /**if(response['RowSat']===1){
          activeClue("row"+i)
        }
        if(response['RowSat']===0){
          inactiveClue("row"+i)
        }
    
        if(response['ColSat']===1){
          activeClue("col"+j)
        }
        if(response['ColSat']===0){
          inactiveClue("col"+j)
        }**/
      }
      setWaiting(false);
    });
  
    

  }

  // funcion que permite saber si el toggle esta en X (true) o en # (false) 
  const [checked, setChecked] = React.useState(false);
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
        rowsCluesSat = {rowsCluesSat}
        colsCluesSat = {colsCluesSat}
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
            <p>{checked.toString()}</p>
        </div>
      </div>
    </div>
  );
}

export default Game;