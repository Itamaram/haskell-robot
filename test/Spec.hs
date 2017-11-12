import Test.Hspec
import Robot

t5 = Table 5

step :: (Maybe Robot, [String]) -> Command -> (Maybe Robot, [String]) 
step (mr, ss) c = case ms of
  Nothing -> (mr', ss)
  Just s  -> (mr', ss ++ [s])
  where (mr', ms) = run mr t5 c

mockRun :: [Command] -> [String]
mockRun cs = snd $ foldl step (Nothing, []) cs 

main :: IO ()
main = hspec $ do
  describe "onTheTable'" $ do
    it "returns true for EQ zero" $ do
      onTheTable' 0 t5 `shouldBe` True
    it "returns false for EQ bound" $ do
      onTheTable' 5 t5 `shouldBe` False
    it "returns true for in bound" $ do
      onTheTable' 2 t5 `shouldBe` True
    it "returns false for LT zero" $ do
      onTheTable' (-1) t5 `shouldBe` False
    it "returns false for GT bound" $ do
      onTheTable' 6 t5 `shouldBe` False
    
  describe "onTheTable" $ do
    it "returns false if x is off" $ do
      onTheTable (-1, 0) t5 `shouldBe` False
    it "returns false if y is off" $ do
      onTheTable (0, -1) t5 `shouldBe` False
    it "returns true if both are on the table" $ do
      onTheTable (0, 0) t5 `shouldBe` True
  
  describe "move'" $ do
    it "move N" $ do
      move' (0, 0) N `shouldBe` (0, 1)
    it "move S" $ do
      move' (0, 0) S `shouldBe` (0, -1)
    it "move W" $ do
      move' (0, 0) W `shouldBe` (-1, 0)
    it "move E" $ do
      move' (0, 0) E `shouldBe` (1, 0)

  describe "Place Command" $ do
    it "place nothing on board" $ do
      run Nothing t5 (Place 1 2 E) `shouldBe` (Just $ Robot 1 2 E, Nothing)
    it "place existing on different spot" $ do
      run (Just $ Robot 1 2 E) t5 (Place 3 4 N) `shouldBe` (Just $ Robot 3 4 N, Nothing)
    it "place nothing off the table" $ do
      run Nothing t5 (Place 0 5 N) `shouldBe` (Nothing, Nothing)
    it "place existing off the table" $ do
      run (Just $ Robot 1 2 E) t5 (Place 0 5 N) `shouldBe` (Just $ Robot 1 2 E, Nothing)
    
  describe "Move Command" $ do
    it "doesn't place a robot on the board" $ do
      run Nothing t5 Move `shouldBe` (Nothing, Nothing)
    it "moves a robot on the board" $ do
      run (Just $ Robot 0 0 N) t5 Move `shouldBe` (Just $ Robot 0 1 N, Nothing)
    it "doesn't let a robot fall off" $ do
      run (Just $ Robot 0 0 S) t5 Move `shouldBe` (Just $ Robot 0 0 S, Nothing)

  describe "Left Command" $ do
    it "doesn't place a robot on the board" $ do
      run Nothing t5 TurnLeft `shouldBe` (Nothing, Nothing)
    it "rotates a robot on the board to the left" $ do
      run (Just $ Robot 0 0 N) t5 TurnLeft `shouldBe` (Just $ Robot 0 0 W, Nothing)

  describe "Right Command" $ do
    it "doesn't place a robot on the board" $ do
      run Nothing t5 TurnRight `shouldBe` (Nothing, Nothing)
    it "rotates a robot on the board to the right" $ do
      run (Just $ Robot 0 0 N) t5 TurnRight `shouldBe` (Just $ Robot 0 0 E, Nothing)

  describe "Report Command" $ do
    it "doesn't report a robot off the board" $ do
      run Nothing t5 Report `shouldBe` (Nothing, Nothing)
    it "reports a robot on the board" $ do
      run (Just $ Robot 1 2 E) t5 Report `shouldBe` (Just $ Robot 1 2 E, Just "1, 2, EAST")

  describe "Examples" $ do
    it "First" $ do
      mockRun [Place 0 0 N, Move, Report] `shouldBe` ["0, 1, NORTH"]
    it "Second" $ do
      mockRun [Place 0 0 N, TurnLeft, Report] `shouldBe` ["0, 0, WEST"]
    it "Third" $ do
      mockRun [Place 1 2 E, Move, Move, TurnLeft, Move, Report] `shouldBe` ["3, 3, NORTH"]

  describe "parsePlace" $ do
    it "Parse Move" $ do
      parseCommand "MOVE" `shouldBe` Just Move
    it "Parse TurnLeft" $ do
      parseCommand "LEFT" `shouldBe` Just TurnLeft
    it "Parse TurnRight" $ do
      parseCommand "RIGHT" `shouldBe` Just TurnRight
    it "Parse Report" $ do
      parseCommand "REPORT" `shouldBe` Just Report
    it "Parse Place" $ do
      parseCommand "PLACE 1,2,NORTH" `shouldBe` Just (Place 1 2 N)
    it "Fail to parse Place" $ do
      parseCommand "PLACE 1,2,AFRICA" `shouldBe` Nothing
    it "Fail to parse Command" $ do
      parseCommand "BARRELROLL" `shouldBe` Nothing
