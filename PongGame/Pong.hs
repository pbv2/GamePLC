-------------------------------------------------- IMPORTS NECESSÁRIOS --------------------------------------------------
 
import Control.Concurrent.MVar
import Graphics.Gloss.Interface.IO.Game


-------------------------------------------------- CRIANDO TYPES E DATAS NECESSÁRIOS --------------------------------------------------

type Control = MVar Integer
type Position = (Float, Float) 

-- Coordenadas do Objeto, bem como suas velocidades nos eixos coordenados
data Object = Object { coordinate_X :: Float, coordinate_Y :: Float,
                       velocity_X :: Float,   velocity_Y :: Float }

-- Atributos necessários para o game
    -- Score / result / isFinish:           Usados para o controle da pontuação e encerramento do game
    -- ball:                                Objeto 'Ball' do game
    -- paddle_player1 / paddle_player2:     Objetos 'Paddles' controlados pelos Players
data PongGame = NewPongGame { score :: (Integer, Integer), result :: Integer,
                              isFinish :: Bool, ball :: Object, gameType :: String,
                              paddle_player1 :: Object, paddle_player2 :: Object }


-------------------------------------------------- VARIÁVEIS ESTATICAS --------------------------------------------------

-- Resoluções usadas no game (Preferênciais, pode usar qualquer outra) --
maximumResolutionWidth, maximumResolutionHeigth :: Int
maximumResolutionWidth  = 1920
maximumResolutionHeigth = 1080
hdResolutionWidth  = 1280
hdResolutionHeigth = 720

-- Configurações do Display
    -- widthDisplay:                                Largura do Display
    -- heightDisplay:                               Altura do Display
    -- fps:                                         Quadros por segundo (Determina a taxa de atualização do Display)
    -- widthDisplayOffset / heightDisplayOffset:    Deslocamento do Display (Ao iniciar o game)
widthDisplay, heightDisplay, fps, widthDisplayOffset, heightDisplayOffset :: Int
widthDisplay  = hdResolutionWidth
heightDisplay = hdResolutionHeigth
fps = 60
widthDisplayOffset  = (div maximumResolutionWidth 2) - (div widthDisplay 2)
heightDisplayOffset = (div maximumResolutionHeigth 2) - (div heightDisplay 2)

-- Placar máximo
maximumScore :: Integer
maximumScore = 3

-- Valores para ajustar o placa no centro do Display
    -- positionScoreHeight:     Valor da altura
    -- positionScoreText:       Deslocamento do placa (em Y)
    -- positionScoreWon:        Deslocamento da mensagem de vitória (em Y)
positionScoreText, positionScoreWon :: Float
positionScoreText   = (-550)
positionScoreWon    = (-430)

-- Cores
ballColor, paddleColor, scoreColor, wallColor, backgroundColor, menuColor :: Color
ballColor   = dark azure
paddleColor = dark white
scoreColor  = light white
menuColor   = light aquamarine
wallColor   = makeColor 0.7 0.7 0.7 1
backgroundColor = dark black

-- Configurações do Wall
    -- wallHeight:      Espessura do Wall
    -- wallOffSet:      Deslocamento, usado para especificar a posição do Wall na parte superior e inferior do Display (Deslocamento em Y)
wallHeight, wallOffSet :: Float
wallHeight = 10
wallOffSet = (fromIntegral (div (heightDisplay) 2))

-- Configurações do Paddle
    -- paddleWidth:                 Espessura do Paddle
    -- paddleHeight:                Comprimento do Paddle
    -- paddleMaximumVelocity:       Velocidade dos Paddles
    -- paddleUpdateVelocityBall:    Taxa de atualização da velocidade da Ball ao rebater nos Paddles
    -- paddleCoordinateX:           Posição fixa (em X) dos Paddles
    -- offSetPaddle:                Usado para bloquear o Paddle no Wall
paddleWidth, paddleHeight, paddleMaximumVelocity, paddleUpdateVelocityBall, paddleCoordinateX, offSetPaddle, offSetPaddleEasy :: Float
paddleWidth  = 20
paddleHeight = 100
paddleMaximumVelocity = 500
paddleUpdateVelocityBall = 0.1
paddleCoordinateX = fromIntegral ((div widthDisplay 2) - 10)
offSetPaddle = (wallOffSet - (paddleHeight / 2))
offSetPaddleEasy = (wallOffSet - paddleHeight)

-- Configurações da Ball
    -- ballRay:             Raio da Ball
    -- ballInitialVelocity: Velocidade inicial da Ball (Ela é incrementada no decorrer do tempo)
ballRay, ballInitialVelocity :: Float
ballRay = 10
ballInitialVelocity = 200


-------------------------------------------------- CRIANDO INSTÂNCIA E ATUALIZANDO GAME --------------------------------------------------

-- Cria o Display do game, a tela do jogo
display :: Display
display = InWindow "Pong - PLC GAME" (widthDisplay, heightDisplay) (widthDisplayOffset, heightDisplayOffset)

-------------------- Cria nova instância do game Pong --------------------
pongGameInit :: PongGame
pongGameInit = NewPongGame
            { score = (0, 0), result = 0, isFinish = False,
              ball = (Object 10000 10000 0 0),
              gameType = "Init",
              paddle_player1 = (Object (-10000) 0 0 0),
              paddle_player2 = (Object 10000    0 0 0) }

pongGameEasy :: PongGame
pongGameEasy = NewPongGame
            { score = (0, 0), result = 0, isFinish = False,
              ball = (Object 0 0 ballInitialVelocity ballInitialVelocity),
              gameType = "Easy",
              paddle_player1 = (Object (-paddleCoordinateX) 0 0 0),
              paddle_player2 = (Object paddleCoordinateX    0 0 0) }

pongGameMedium :: PongGame
pongGameMedium = NewPongGame
            { score = (0, 0), result = 0, isFinish = False,
              ball = (Object 0 0 ballInitialVelocity ballInitialVelocity),
              gameType = "Medium",
              paddle_player1 = (Object (-paddleCoordinateX) 0 0 0),
              paddle_player2 = (Object paddleCoordinateX    0 0 0) }

pongGameHard :: PongGame
pongGameHard = NewPongGame
            { score = (0, 0), result = 0, isFinish = False,
              ball = (Object 0 0 (2*ballInitialVelocity) (2 * ballInitialVelocity)),
              gameType = "Hard",
              paddle_player1 = (Object (-paddleCoordinateX) 0 0 0),
              paddle_player2 = (Object paddleCoordinateX    0 0 0) }

-- Converte instância atual do game em uma imagem para ser exibida no Display --
paintingTheScreen :: (PongGame, Control, Control) -> IO Picture
paintingTheScreen (pongGame, _, _) = return pictureGame
    where
        pictureGame = if (gameType pongGame /= "Init")
                        then Pictures [ makeScore player1_Score player2_Score resultScore,
                                        makeBall coordinate_X_Ball coordinate_Y_Ball,
                                        makePaddle pongGame coordinate_X_Paddle_Player_1 coordinate_Y_Paddle_Player_1,
                                        makePaddle pongGame coordinate_X_Paddle_Player_2 coordinate_Y_Paddle_Player_2,
                                        makeWall wallOffSet, makeWall (-wallOffSet) ]
                        else Pictures [ makeMenu "Pong - PLC GAME"                  ((-coordinate_X) * scale_Coordinate_X * 0.7) ((fromIntegral heightDisplay) - coordinate_Y)         (scale * 1.5)  (scale * 1.5),
                                        makeMenu "Selecione a dificuldade abaixo:"  ((-coordinate_X) * scale_Coordinate_X * 1) ((fromIntegral heightDisplay) - (1.5 * coordinate_Y)) (scale * 1.25) (scale * 1.25),
                                        makeMenu "Easy - F1"                        ((-coordinate_X) * 0.5)                      ((fromIntegral heightDisplay) - (2.5 * coordinate_Y)) scale scale,
                                        makeMenu "Normal - F2"                      ((-coordinate_X) * 0.5)                      ((fromIntegral heightDisplay) - (3.5 * coordinate_Y)) scale scale,
                                        makeMenu "Hard - F3"                        ((-coordinate_X) * 0.5)                      ((fromIntegral heightDisplay) - (4.5 * coordinate_Y)) scale scale,
                                        makeMenu "Use F5 para reiniciar o jogo"     ((-coordinate_X) * scale_Coordinate_X)       ((fromIntegral heightDisplay) - (7 * coordinate_Y))   scale scale,
                                        makeMenu "Use F7 para voltar ao Menu"       ((-coordinate_X) * scale_Coordinate_X)       ((fromIntegral heightDisplay) - (8 * coordinate_Y))   scale scale,
                                        makeBall coordinate_X_Ball coordinate_Y_Ball,
                                        makePaddle pongGame coordinate_X_Paddle_Player_1 coordinate_Y_Paddle_Player_1,
                                        makePaddle pongGame coordinate_X_Paddle_Player_2 coordinate_Y_Paddle_Player_2,
                                        makeWall wallOffSet, makeWall (-wallOffSet) ]

        -- Placar inicial --
        (player1_Score, player2_Score) = score pongGame
        resultScore = result pongGame

        -- Coordenadas da Ball --
        (Object coordinate_X_Ball coordinate_Y_Ball _ _) = ball pongGame

        -- Coordenadas dos Paddles --
        (Object coordinate_X_Paddle_Player_1 coordinate_Y_Paddle_Player_1 _ _) = paddle_player1 pongGame
        (Object coordinate_X_Paddle_Player_2 coordinate_Y_Paddle_Player_2 _ _) = paddle_player2 pongGame

        -- Variaveis para ajustar mensagens exibidas no Menu --
        coordinate_X = fromIntegral (div widthDisplay 2)
        coordinate_Y = fromIntegral (div heightDisplay 4)
        scale_Coordinate_X = (fromIntegral maximumResolutionWidth) / (fromIntegral (widthDisplay))
        scale = 1280 / ((fromIntegral maximumResolutionWidth) * 2)

-- Atualiza instância atual do game, criando uma nova
updateSteps :: Float -> (PongGame, Control, Control) -> IO (PongGame, Control, Control)
updateSteps time (pongGame, player1_Control, player2_Control) = return ((updateBall $ movePaddles time $ moveBall time pongGame), player1_Control, player2_Control)


-------------------------------------------------- FUNÇÕES MAKE (SCORE, WALL, PADDLE, BALL) --------------------------------------------------

-- Cria as mensagens exibidas no Menu (Inicio do game) --
makeMenu :: String -> Float -> Float -> Float -> Float -> Picture
makeMenu text positionMenuWidth positionMenuHeight scaleX scaleY = scale scaleX scaleY (Translate positionMenuWidth positionMenuHeight $ Color menuColor $ Text text)

-- player1_Score / player2_Score:           Pontuações dos players
    -- result:                              Variável que determina os resultados
                                        --      0  -> Ainda em game
                                        --      1  -> Vitória do player 1
                                        --      10 -> Vitória do player 2
-- positionScoreWidth / positionScoreHeight: Valores para ajustar o placa no centro do Display
makeScore :: Integer -> Integer -> Integer -> Picture
makeScore player1_Score player2_Score result = Translate positionScoreWidth positionScoreHeight $ Color scoreColor $ Text text
    where
        text = if (result == 0)
                    then (show player1_Score ++ "         " ++ show player2_Score)
                    else if (result == 10)
                        then ("Jog. 2 ganhou!")
                        else ("jog. 1 ganhou!")

        positionScoreWidth = if (result /= 0)
                                then positionScoreWon
                                else positionScoreText
    
        positionScoreHeight = if (result /= 0)
                                then 0
                                else (fromIntegral (div heightDisplay 3))

-- Aqui o comprimento do Wall é o mesmo que a largura do Display
-- Essa função cria um Wall
makeWall :: Float -> Picture
makeWall offSet = Translate 0 offSet $ Color wallColor $ rectangleSolid (fromIntegral widthDisplay) wallHeight

-- Essa função cria um Paddle
makePaddle :: PongGame -> Float -> Float -> Picture
makePaddle pongGame x y = do if (gameType pongGame == "Easy")
                                then Pictures [ Translate x y $ Color paddleColor $ rectangleSolid paddleWidth (2 * paddleHeight) ]
                                else Pictures [ Translate x y $ Color paddleColor $ rectangleSolid paddleWidth paddleHeight ]

-- Essa função cria uma Ball
makeBall :: Float -> Float -> Picture
makeBall x y = uncurry Translate (x,y) $ Color ballColor $ circleSolid ballRay


-------------------------------------------------- FUNÇÕES MOVE (PADDLE, BALL) --------------------------------------------------

-- Responsável pela movimentação dos Paddles (Calcula nova posição dos Paddles)
movePaddles :: Float -> PongGame -> PongGame
movePaddles time pongGame = pongGame 
    { paddle_player1 = (Object coordinate_X_Paddle_Player_1 coordinate_Y_Paddle_Player_1' velocity_X_Paddle_Player_1 velocity_Y_Paddle_Player_1)
    , paddle_player2 = (Object coordinate_X_Paddle_Player_2 coordinate_Y_Paddle_Player_2' velocity_X_Paddle_Player_2 velocity_Y_Paddle_Player_2) }
        where
            -- Atributos dos Paddles --
            (Object coordinate_X_Paddle_Player_1 coordinate_Y_Paddle_Player_1 velocity_X_Paddle_Player_1 velocity_Y_Paddle_Player_1) = paddle_player1 pongGame
            (Object coordinate_X_Paddle_Player_2 coordinate_Y_Paddle_Player_2 velocity_X_Paddle_Player_2 velocity_Y_Paddle_Player_2) = paddle_player2 pongGame

            -- Ajustando offSetPaddle (Usando para impedir que os Paddles saiam do Display) --
            offSetPaddles = if (gameType pongGame == "Easy") then offSetPaddleEasy else offSetPaddle

            -- Calcula nova posição Y dos Paddles (O Paddle não se desloca no eixo X) --
            newY1 = coordinate_Y_Paddle_Player_1 + velocity_Y_Paddle_Player_1 * time
            newY2 = coordinate_Y_Paddle_Player_2 + velocity_Y_Paddle_Player_2 * time
            
            -- Verificando se os Paddles vão para uma posição fora do Display, fora dos limites dos Walls --
            coordinate_Y_Paddle_Player_1' = if (newY1 > offSetPaddles) then offSetPaddles else if (newY1 < (-offSetPaddles)) then (-offSetPaddles) else newY1
            coordinate_Y_Paddle_Player_2' = if (newY2 > offSetPaddles) then offSetPaddles else if (newY2 < (-offSetPaddles)) then (-offSetPaddles) else newY2

-- Responsável pela movimentação da Ball (Calcula nova posição da Ball)
moveBall :: Float -> PongGame -> PongGame
moveBall time pongGame = pongGame
    { ball = (Object coordinate_X_Ball' coordinate_Y_Ball' velocity_X_Ball velocity_Y_Ball) }
        where
            -- Atributos da Ball --
            (Object coordinate_X_Ball coordinate_Y_Ball velocity_X_Ball velocity_Y_Ball) = ball pongGame

            -- Calcula nova posição da Ball --
            coordinate_X_Ball' = coordinate_X_Ball + velocity_X_Ball * time
            coordinate_Y_Ball' = coordinate_Y_Ball + velocity_Y_Ball * time


-------------------------------------------------- FUNÇÕES DE COLISÃO (WALLS <> BALL, PADDLES <> BALL) --------------------------------------------------

-- Verificando se a Ball chocou com os Walls (Superior e Inferior)
wallCollisionBall :: Position -> Bool
wallCollisionBall (_, coordinate_Y_Ball) = upWallCollision || downWallCollision
    where
        upWallCollision   = coordinate_Y_Ball + ballRay >= wallOffSet
        downWallCollision = coordinate_Y_Ball - ballRay <= (-wallOffSet)

-- Verifica:
    -- (Player 1)
        -- (coordinate_X_Ball - ballRay <= (-paddleCoordinateX))                            /  (coordinate_X_Ball - ballRay >= -(paddleCoordinateX + paddleWidth)):               Verifica se a Ball colidio com Paddle esquerdo em relação a área do eixo X (Entre a parte da frente e de tras da Ball)
        -- (coordinate_Y_Ball >= (paddle_player1_Coordinate_Y - (paddleHeight / divider)))  /  (coordinate_Y_Ball <= ((paddleHeight / divider) + paddle_player1_Coordinate_Y)):   Verifica se a Ball colidio com Paddle esquerdo em relação a área do eixo Y (Entre o ponto mais abaixo do Paddle até o ponto mais acima do Paddle)
    -- (Player 2)
        -- (coordinate_X_Ball + ballRay >= paddleCoordinateX)                               /  (coordinate_X_Ball + ballRay <= (paddleCoordinateX + paddleWidth)):                Verifica se a Ball colidio com Paddle direito em relação a área do eixo X (Entre a parte da frente e de tras da Ball)
        -- (coordinate_Y_Ball >= (paddle_player2_Coordinate_Y - (paddleHeight / divider)))  /  (coordinate_Y_Ball <= ((paddleHeight / divider) + paddle_player2_Coordinate_Y)):   Verifica se a Ball colidio com Paddle direito em relação a área do eixo Y (Entre o ponto mais abaixo do Paddle até o ponto mais acima do Paddle)
paddleCollisionBall :: Position -> Float -> Float -> String -> Bool
paddleCollisionBall (coordinate_X_Ball, coordinate_Y_Ball) paddle_player1_Coordinate_Y paddle_player2_Coordinate_Y gameType = player1PaddleCollisionBall || player2PaddleCollisionBall
    where
        divider = if (gameType == "Easy") then 1 else 2
        player1PaddleCollisionBall = (coordinate_X_Ball - ballRay <= (-paddleCoordinateX)) && (coordinate_X_Ball - ballRay >= -(paddleCoordinateX + paddleWidth)) && (coordinate_Y_Ball >= (paddle_player1_Coordinate_Y - (paddleHeight / divider))) && (coordinate_Y_Ball <= ((paddleHeight / divider) + paddle_player1_Coordinate_Y))
        player2PaddleCollisionBall = (coordinate_X_Ball + ballRay >= paddleCoordinateX)    && (coordinate_X_Ball + ballRay <= (paddleCoordinateX + paddleWidth))  && (coordinate_Y_Ball >= (paddle_player2_Coordinate_Y - (paddleHeight / divider))) && (coordinate_Y_Ball <= ((paddleHeight / divider) + paddle_player2_Coordinate_Y))
   

-------------------------------------------------- FUNÇÕES USADAS PARA O UPDATE DO SCORE E DA BALL --------------------------------------------------

isUpdateScore :: Float -> Int
isUpdateScore coordinate_X_Ball = 
    if (coordinate_X_Ball > (paddleCoordinateX + paddleWidth)) then 1
    else if coordinate_X_Ball < (-(paddleCoordinateX + paddleWidth)) then 10 else 0

updateBall :: PongGame -> PongGame
updateBall pongGame = pongGame { ball = (Object coordinate_X_Ball' coordinate_Y_Ball' velocity_X_Ball' velocity_Y_Ball'),
                                 score = (player1_Score', player2_Score'),
                                 result = result', isFinish = isFinish' }
    where
    -- Atributos da Ball --
    (Object coordinate_X_Ball coordinate_Y_Ball velocity_X_Ball velocity_Y_Ball) = ball pongGame
                
    -- Scores --
    (player1_Score, player2_Score) = score pongGame
    
    -- Atributos dos Paddles --
    (Object coordinate_X_Paddle_Player_1 coordinate_Y_Paddle_Player_1 _ _) = paddle_player1 pongGame
    (Object coordinate_X_Paddle_Player_2 coordinate_Y_Paddle_Player_2 _ _) = paddle_player2 pongGame
                 
    -- Tipo do game (Easy, Medium, High) --
    typeGame = gameType pongGame

    -- Calculando possiveis colisões (Paddles <> Ball | Walls <> Ball) --
    ispaddleCollisionBall = paddleCollisionBall (coordinate_X_Ball, coordinate_Y_Ball) coordinate_Y_Paddle_Player_1 coordinate_Y_Paddle_Player_2 typeGame
    isWallCollisionBall   = wallCollisionBall   (coordinate_X_Ball, coordinate_Y_Ball)
    
    -- Atualizando pontuação (Verificando se algum Player pontuou)
    points = isUpdateScore coordinate_X_Ball

    -- Atualizando placar dos Players
    player1_Score' = if ((isFinish pongGame) == False && (points == 1))
                        then (player1_Score + 1)
                        else player1_Score
    
    player2_Score' = if ((isFinish pongGame) == False && (points == 10))
                        then (player2_Score + 1)
                        else player2_Score
    
    -- Verificando se houve algum vencedor --
    result' = if (player1_Score' == maximumScore)
                then 1
                else if player2_Score' == maximumScore
                    then 10
                    else 0
    
    -- Atualizando flag de finalização do game --
    isFinish' = if (result' == 0) then False else True
    
    -------------------- Atualizando a Ball --------------------

    -- Atualizando coordenadas (Vai para centro do Display caso algum dos Players tenha pontuado) --
    coordinate_X_Ball' = if (points == 0) then coordinate_X_Ball else if (typeGame == "Init") then coordinate_X_Ball else 0
                        
    coordinate_Y_Ball' = if (points == 0) then coordinate_Y_Ball else if (typeGame == "Init") then coordinate_X_Ball else 0
    
    -- Atualizando velocidade --
    velocity_X_Ball' = if (points /= 0)
                            then if (typeGame == "Hard")
                                then if (points == 1)
                                    then (2 * ballInitialVelocity)    -- + (ballInitialVelocity * ((fromIntegral (player1_Score' + player2_Score')) / (fromIntegral (maximumScore * 2))))
                                    else (2 * (-ballInitialVelocity)) -- - (ballInitialVelocity * ((fromIntegral (player1_Score' + player2_Score')) / (fromIntegral (maximumScore * 2))))
                                else if (points == 1)
                                    then ballInitialVelocity          -- + (ballInitialVelocity * ((fromIntegral (player1_Score' + player2_Score')) / (fromIntegral (maximumScore * 2))))
                                    else (-ballInitialVelocity)       -- - (ballInitialVelocity * ((fromIntegral (player1_Score' + player2_Score')) / (fromIntegral (maximumScore * 2))))
                            else if ispaddleCollisionBall
                                then (-(velocity_X_Ball + (velocity_X_Ball * paddleUpdateVelocityBall)))
                                else velocity_X_Ball
    
    velocity_Y_Ball' = if (points /= 0)
                            then if (typeGame == "Hard")
                                then (2 * ballInitialVelocity) -- - (ballInitialVelocity * ((fromIntegral (player1_Score' + player2_Score')) / (fromIntegral (maximumScore * 2))))
                                else ballInitialVelocity       -- - (ballInitialVelocity * ((fromIntegral (player1_Score' + player2_Score')) / (fromIntegral (maximumScore * 2))))
                            else if ispaddleCollisionBall
                                then (velocity_Y_Ball + (velocity_Y_Ball * paddleUpdateVelocityBall))
                                else if isWallCollisionBall
                                    then (-velocity_Y_Ball)
                                    else velocity_Y_Ball


-------------------------------------------------- AÇÕES DO GAME (EVENTOS) --------------------------------------------------

-- Funcionalidades / Opções do PongGame --
gameEvents :: Event -> (PongGame, Control, Control) -> IO (PongGame, Control, Control)
-- Seleção de dificuldade --
gameEvents (EventKey (SpecialKey KeyF1) _ _ _) (pongGame, player1_Control, player2_Control) = do
    return (pongGameEasy, player1_Control, player2_Control)

gameEvents (EventKey (SpecialKey KeyF2) _ _ _) (pongGame, player1_Control, player2_Control) = do
    return (pongGameMedium, player1_Control, player2_Control)

gameEvents (EventKey (SpecialKey KeyF3) _ _ _) (pongGame, player1_Control, player2_Control) = do
    return (pongGameHard, player1_Control, player2_Control)

gameEvents (EventKey (SpecialKey KeyF7) _ _ _) (pongGame, player1_Control, player2_Control) = do
    return (pongGameInit, player1_Control, player2_Control)

-- Reinicia o game --
gameEvents (EventKey (SpecialKey KeyF5) _ _ _) (pongGame, player1_Control, player2_Control) = do
    if ((gameType pongGame) == "Easy")
        then return (pongGameEasy, player1_Control, player2_Control)
    else if ((gameType pongGame) == "Medium")
        then return (pongGameMedium, player1_Control, player2_Control)
    else if ((gameType pongGame) == "Hard")
        then return (pongGameHard, player1_Control, player2_Control)
    else return (pongGameInit, player1_Control, player2_Control)

-- Capturando ações do Teclado (Botão sendo precionado) --
gameEvents (EventKey key Down _ _) (pongGame, player1_Control, player2_Control) = do
    -- Opções validas para o Player 1 (Esquerda)
    if ((key == (Char 's')) || (key == (Char 'S')) || (key == (Char 'w')) || (key == (Char 'W')))
        then do
        c <- takeMVar player1_Control
        putMVar player1_Control (c+c)

        if ((key == (Char 's')) || (key == (Char 'S')))
            then return $ (pongGame { paddle_player1 = (Object coordinate_X_Paddle_Player_1 coordinate_Y_Paddle_Player_1 0 (-paddleMaximumVelocity)) }, player1_Control, player2_Control)
            else return $ (pongGame { paddle_player1 = (Object coordinate_X_Paddle_Player_1 coordinate_Y_Paddle_Player_1 0 (paddleMaximumVelocity))  }, player1_Control, player2_Control)
    
    -- Opções validas para o Player 2 (Direita)
    else if ((key == (SpecialKey KeyUp)) || (key == (SpecialKey KeyDown)))
        then do
        c <- takeMVar player2_Control
        putMVar player2_Control (c+c)
        
        if (key == (SpecialKey KeyUp))
            then return $ (pongGame { paddle_player2 = (Object coordinate_X_Paddle_Player_2 coordinate_Y_Paddle_Player_2 0 (paddleMaximumVelocity))  }, player1_Control, player2_Control)
            else return $ (pongGame { paddle_player2 = (Object coordinate_X_Paddle_Player_2 coordinate_Y_Paddle_Player_2 0 (-paddleMaximumVelocity)) }, player1_Control, player2_Control)
        
    -- Ação que não faz nada (Nenhuma das opções acima foi selecionada) --
    else return $ (pongGame { paddle_player1 = (Object coordinate_X_Paddle_Player_1 coordinate_Y_Paddle_Player_1 0 0) } , player1_Control, player2_Control)
        where
            (Object coordinate_X_Paddle_Player_1 coordinate_Y_Paddle_Player_1 _ _) = paddle_player1 pongGame
            (Object coordinate_X_Paddle_Player_2 coordinate_Y_Paddle_Player_2 _ _) = paddle_player2 pongGame

-- Capturando ações do Teclado (Botão voltando ao normal) --
gameEvents (EventKey key Up _ _) (pongGame, player1_Control, player2_Control) = do
    -- Opções validas para o Player 1 (Esquerda)
    if ((key == (Char 's')) || (key == (Char 'S')) || (key == (Char 'w')) || (key == (Char 'W'))) then do
        c <- takeMVar player1_Control
        putMVar player1_Control (c-c)

        if ((c-c) /= 0)
            then if ((key == (Char 's')) || (key == (Char 'S'))) then return $ (pongGame { paddle_player1 = (Object coordinate_X_Paddle_Player_1 coordinate_Y_Paddle_Player_1 0 (paddleMaximumVelocity))  }, player1_Control, player2_Control)
                                        else return $ (pongGame { paddle_player1 = (Object coordinate_X_Paddle_Player_1 coordinate_Y_Paddle_Player_1 0 (-paddleMaximumVelocity)) }, player1_Control, player2_Control)
            else return $ (pongGame { paddle_player1 = (Object coordinate_X_Paddle_Player_1 coordinate_Y_Paddle_Player_1 0 0) }, player1_Control, player2_Control)

    -- Opções validas para o Player 2 (Direita)
    else if ((SpecialKey KeyUp) == key || (SpecialKey KeyDown) == key) then do
        c <- takeMVar player2_Control
        putMVar player2_Control (c-c)

        if ((c-c) /= 0)
            then if ((SpecialKey KeyUp) == key) then return $ (pongGame { paddle_player2 = (Object coordinate_X_Paddle_Player_2 coordinate_Y_Paddle_Player_2 0 (-paddleMaximumVelocity)) }, player1_Control, player2_Control)
                                                else return $ (pongGame { paddle_player2 = (Object coordinate_X_Paddle_Player_2 coordinate_Y_Paddle_Player_2 0 (paddleMaximumVelocity))  }, player1_Control, player2_Control)
            else return $ (pongGame { paddle_player2 = (Object coordinate_X_Paddle_Player_2 coordinate_Y_Paddle_Player_2 0 0) }, player1_Control, player2_Control)
    
    -- Ação que não faz nada (Nenhuma das opções acima foi selecionada) --
    else return $ (pongGame, player1_Control, player2_Control)
        where
            (Object coordinate_X_Paddle_Player_1 coordinate_Y_Paddle_Player_1 _ _) = paddle_player1 pongGame
            (Object coordinate_X_Paddle_Player_2 coordinate_Y_Paddle_Player_2 _ _) = paddle_player2 pongGame

-- Opções invalidas (Ainda não configuradas) --
gameEvents _ (pongGame, player1_Control, player2_Control) = return $ (pongGame, player1_Control, player2_Control)


-------------------------------------------------- MAIN --------------------------------------------------

-- display:                                         Display do game
-- backgroundColor:                                 Cor de fundo
-- fps:                                             Taxa de atualização do Display
-- (pongGame, player1_Control, player2_Control):    Nova instância do game, juntamento com o controle dos players
-- paintingTheScreen:                               Criando objetos na tela
-- gameEvents:                                      Eventos (ações) que ocorrem durante o game
-- updateSteps:                                     Gerador dos passos (iteração) do game

main :: IO()
main = do
    player1_Control <- newMVar 0
    player2_Control <- newMVar 0
    playIO display backgroundColor fps (pongGameInit, player1_Control, player2_Control) paintingTheScreen gameEvents updateSteps