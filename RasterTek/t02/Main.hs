import Engine

main = do
    engine <- Engine.initialize "Tutorial 2" 800 600
    Engine.run engine
    Engine.shutdown engine

