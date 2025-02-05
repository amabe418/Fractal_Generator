import { useState } from 'react'
import reactLogo from './assets/react.svg'
import viteLogo from '/vite.svg'
import './App.css'
import FractalGenerator from './components/fractal-frontend'

function App() {
  const [count, setCount] = useState(0)
  
  return (
    <>
      <div>  
        <FractalGenerator/>
      </div>
    </>
  )
}

export default App
