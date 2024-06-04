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
  const [checked, setChecked] = React.useState(true);
  const [ganador, setGanador] = React.useState(false);
  const [solucion, setSolucion] = useState(null);
  useEffect(() => {
    // Creation of the pengine server instance.    
    // This is executed just once, after the first render.    
    // The callback will run when the server is ready, and it stores the pengine instance in the pengine variable. 
    PengineClient.init(handleServerReady);
  }, []);

  function handleServerReady(instance) {
    pengine = instance;
    const queryS = 'init(RowClues, ColumClues, Grid), tableroInicial(RowClues, ColumClues, Grid, RowsCluesSat, ColsCluesSat)';
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['Grid']);
        setRowsClues(response['RowClues']);
        setColsClues(response['ColumClues']);
        setRowsCluesSat(response['RowsCluesSat']);
        setColsCluesSat(response['ColsCluesSat']);
        const squaresS = JSON.stringify(response['Grid']);
        const rowsCluesS = JSON.stringify(response['RowClues']);
        const colsCluesS = JSON.stringify(response['ColumClues']);
        const rowsCluesSS = JSON.stringify(response['RowsCluesSat']);
        const colsCluesSS = JSON.stringify(response['ColsCluesSat']);
        const querySS = `resolverNonograma(${squaresS},${rowsCluesS}, ${colsCluesS},${rowsCluesSS}, ${colsCluesSS},Solucion)`;
        pengine.query(querySS, (success, response) => {
          if (success) {
            setSolucion(response['Solucion']);
            console.log(response['Solucion']);
          }
      });
      
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
    const rowsCluesS = JSON.stringify(rowsClues);
    const colsCluesS = JSON.stringify(colsClues);
    
    const queryS = `put("${content}", [${i},${j}], ${rowsCluesS}, ${colsCluesS}, ${squaresS}, ResGrid, RowSat, ColSat)`; // queryS = put("#",[0,1],[], [],[["X",_,_,_,_],["X",_,"X",_,_],["X",_,_,_,_],["#","#","#",_,_],[_,_,"#","#","#"]], GrillaRes, FilaSat, ColSat)
    setWaiting(true);
    
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['ResGrid']);
        rowsCluesSat[i] = response['RowSat'];
        colsCluesSat[j] = response['ColSat'];
      }
      const rowsCluesSatS = JSON.stringify(rowsCluesSat);
      const colsCluesSatS = JSON.stringify(colsCluesSat);
      const queryG = `checkGanador(${rowsCluesSatS},${colsCluesSatS},Ganador)`;
      pengine.query(queryG, (success, response) => {
        if (success) {
          setGanador(response['Ganador']);
        }
      });
      setWaiting(false);
    });
 
  }

  const handleChange = () => {
    setChecked(!checked);
  };

  if (!grid) {
    return null;
  }
 
  return (
    <div className="game">
      <div className="board-container">
        <Board
          grid={grid}
          rowsClues={rowsClues}
          colsClues={colsClues}
          rowsCluesSat={rowsCluesSat}
          colsCluesSat={colsCluesSat}
          ganador={ganador}
          onClick={(i, j) => handleClick(i, j, checked)}
        />
      </div>
      <div className="game-info">
        <div className="settings">
          <label className="switch" >
            <input
              type="checkbox"
              checked={checked}
              onChange={handleChange}
              disabled = {ganador}
            />
            <span className="slider round"></span>
          </label>
        </div>
      </div>
    </div>
  );
}

export default Game;