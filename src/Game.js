import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import { faLightbulb } from '@fortawesome/free-solid-svg-icons';

let pengine;

function Game() {
  // State
  const [grid, setGrid] = useState(null);
  const [rowsClues, setRowsClues] = useState(null);
  const [colsClues, setColsClues] = useState(null);
  const [waiting, setWaiting] = useState(false);
  const [rowsCluesSat, setRowsCluesSat] = useState([]);
  const [colsCluesSat, setColsCluesSat] = useState([]);
  const [checked, setChecked] = useState(true);
  const [clueActive, setClueActive] = useState(false);
  const [ganador, setGanador] = useState(false);
  const [solucion, setSolucion] = useState(null);
  const [showSolution, setShowSolution] = useState(false); // Nuevo estado

  useEffect(() => {
    // Creación de la instancia del servidor Pengine.
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
        const querySS = `resolverNonograma(${squaresS},${rowsCluesS}, ${colsCluesS},Solucion)`;
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
    if (waiting || showSolution) return; // No permitir cambios si se está mostrando la solución

    const squaresS = JSON.stringify(grid).replaceAll('"_"', '_');
    let content = checked ? '#' : 'X';
    const rowsCluesS = JSON.stringify(rowsClues);
    const colsCluesS = JSON.stringify(colsClues);

    const queryS = `put("${content}", [${i},${j}], ${rowsCluesS}, ${colsCluesS}, ${squaresS}, ResGrid, RowSat, ColSat)`;
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

  const toggleSolution = () => {
    setShowSolution(!showSolution);
  };

  if (!grid) {
    return null;
  }

  return (
    <div className="game">
      <div className="board-container">
        <Board
          grid={showSolution ? solucion : grid}
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
          <label className="solucion">
            <input
              type="checkbox"
              checked={showSolution}
              onChange={toggleSolution}
            />
            <span className="solucionSwitch">
              Solución
            </span>
          </label>
          <label className="switch">
            <input
              type="checkbox"
              checked={checked}
              onChange={handleChange}
              disabled={ganador}
            />
            <span className="slider round"></span>
          </label>
          <label className="clueLabel">
            <input
              type="checkbox"
            />
            <span className="clueSwitch">
              <FontAwesomeIcon icon={faLightbulb} className="lightbulb-icon" />
            </span>
          </label>
        </div>
      </div>
    </div>
  );
}

export default Game;
