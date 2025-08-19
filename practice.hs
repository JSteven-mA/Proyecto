import Data.Time.Clock 
import Data.List 
import System.IO 
import Control.Exception 
import Control.Concurrent (threadDelay) 
import Data.Maybe (mapMaybe, isNothing) 
import System.Directory (doesFileExist) 
import Control.DeepSeq (force)
import Control.Exception (evaluate, IOException, try)

-- Definición del tipo Libro con ID, fecha de préstamo y fecha de devolución opcional
data Libro = Libro {
    iD :: String,                -- Identificador único del libro
    prestado :: UTCTime,         -- Fecha y hora en que se prestó el libro
    devuelto :: Maybe UTCTime    -- Fecha y hora en que se devolvió el libro (puede ser Nothing)
} deriving (Show, Read) 

-- Registra un nuevo préstamo agregando el libro a la lista
registrarPrestamo :: String -> UTCTime -> [Libro] -> [Libro] 
registrarPrestamo idLibro tiempo biblioteca =
    Libro idLibro tiempo Nothing : biblioteca 

-- Marca un libro como devuelto actualizando su campo 'devuelto'
registrarDevolucion :: String -> UTCTime -> [Libro] -> [Libro] 
registrarDevolucion idLibro tiempo biblioteca =
    map (\l -> if idLibro == iD l then l { devuelto = Just tiempo } else l) biblioteca 

-- Busca un libro por ID que aún no haya sido devuelto
buscarLibro :: String -> [Libro] -> Maybe Libro 
buscarLibro idLibro biblioteca =
    find (\l -> idLibro == iD l && isNothing (devuelto l)) biblioteca 

-- Calcula el tiempo que el libro ha estado prestado
tiempoPrestado :: Libro -> UTCTime -> NominalDiffTime 
tiempoPrestado libro tiempoActual =
    case devuelto libro of
        Just tiempoDevuelto -> diffUTCTime tiempoDevuelto (prestado libro)
        Nothing             -> diffUTCTime tiempoActual (prestado libro) 

-- Intenta ejecutar una acción IO varias veces si ocurre una excepción
reintentar :: Int -> IO a -> IO (Either IOException a) 
reintentar 0 accion = catch (accion >>= return . Right) (\(ex :: IOException) -> return (Left ex)) 
reintentar n accion = do
    resultado <- catch (accion >>= return . Right) (\(ex :: IOException) -> return (Left ex)) 
    case resultado of
        Left _ -> do
            threadDelay 1000000
            reintentar (n - 1) accion
        Right val -> return (Right val) 

-- Guarda la lista de libros en el archivo 'Library.txt' con reintentos en caso de error
guardarBiblioteca :: [Libro] -> IO () 
guardarBiblioteca biblioteca = do
    let contenido = unlines (map show biblioteca)
    resultado <- reintentar 5 (writeFile "Library.txt" contenido)
    case resultado of
        Left ex -> putStrLn $ "Error guardando la biblioteca: " ++ show ex
        Right _ -> putStrLn "Biblioteca guardada en el archivo Library.txt." 

-- Carga la lista de libros desde el archivo 'Library.txt'
cargarBiblioteca :: IO [Libro] 
cargarBiblioteca = do
    let archivo = "Library.txt"
    existe <- doesFileExist archivo
    if not existe
        then writeFile archivo "" >> return []
        else do
            eContenido <- try (readFile archivo) :: IO (Either IOException String)
            case eContenido of
                Left _ -> return []
                Right contenido -> do
                    -- Forzar lectura completa para liberar el handle del archivo
                    _ <- evaluate (force contenido)
                    let lineas = filter (not . null) (lines contenido)
                    return (map read lineas) 

-- Lee y parsea la lista de libros desde el archivo
leerBiblioteca :: IO [Libro] 
leerBiblioteca = do
    eContenido <- try (readFile "Library.txt") :: IO (Either IOException String)
    case eContenido of
        Left _ -> return []
        Right contenido -> do
            -- Forzar lectura completa para liberar el handle del archivo
            _ <- evaluate (force contenido)
            let lineas = filter (not . null) (lines contenido)
            return (map read lineas) 

-- Ciclo principal del programa: menú de opciones y lógica de interacción
cicloPrincipal :: [Libro] -> IO () 
cicloPrincipal biblioteca = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar préstamo de libro"
    putStrLn "2. Registrar devolución de libro"
    putStrLn "3. Buscar libro por ID"
    putStrLn "4. Listar los libros de la biblioteca"
    putStrLn "5. Salir"
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el ID del libro:"
            idLibro <- getLine
            tiempoActual <- getCurrentTime
            let bibliotecaActualizada = registrarPrestamo idLibro tiempoActual biblioteca
            putStrLn $ "Libro con ID " ++ idLibro ++ " prestado."
            guardarBiblioteca bibliotecaActualizada
            cicloPrincipal bibliotecaActualizada
        "2" -> do
            putStrLn "Ingrese el ID del libro a devolver:"
            idLibro <- getLine
            tiempoActual <- getCurrentTime
            let bibliotecaActualizada = registrarDevolucion idLibro tiempoActual biblioteca
            putStrLn $ "Libro con ID " ++ idLibro ++ " devuelto."
            guardarBiblioteca bibliotecaActualizada
            cicloPrincipal bibliotecaActualizada
        "3" -> do
            putStrLn "Ingrese el ID del libro a buscar:"
            idLibro <- getLine
            case buscarLibro idLibro biblioteca of
                Just libro -> do
                    tiempoActual <- getCurrentTime
                    let tiempoTotal = tiempoPrestado libro tiempoActual
                    putStrLn $ "El libro con ID " ++ idLibro ++ " está prestado."
                    putStrLn $ "Tiempo prestado: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Libro no encontrado o ya devuelto."
            cicloPrincipal biblioteca
        "4" -> do
            putStrLn "Mostrando lista de libros en la biblioteca"
            bibliotecaActualizada <- leerBiblioteca
            mapM_ (\l -> putStrLn $ "ID: " ++ iD l ++ ", Prestado: " ++ show (prestado l) ++ ", Devuelto: " ++ show (devuelto l)) bibliotecaActualizada
            cicloPrincipal bibliotecaActualizada
        "5" -> putStrLn "¡Hasta luego!"
        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal biblioteca 

-- Punto de entrada principal del programa
main :: IO () 
main = do
    biblioteca <- cargarBiblioteca
    putStrLn "¡Bienvenido al Sistema de Gestión de Biblioteca!"
    cicloPrincipal biblioteca