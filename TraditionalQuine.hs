quote = (toEnum 34)::Char
preCodeLines = 3
code = [
    "quote = (toEnum 34)::Char",
    "preCodeLines = 3",
    "code = [",
    "    ",
    "    ]",
    "main = mapM_ putStrLn (take preCodeLines code) >> mapM_ putStrLn (map ((++[',']).((++) . head . drop preCodeLines $ code) . show) (init code)) >> putStrLn (((++) . head . drop preCodeLines $ code) . show $ last code) >> mapM_ putStrLn (tail . drop preCodeLines $ code)"
    ]
main = mapM_ putStrLn (take preCodeLines code) >> mapM_ putStrLn (map ((++[',']).((++) . head . drop preCodeLines $ code) . show) (init code)) >> putStrLn (((++) . head . drop preCodeLines $ code) . show $ last code) >> mapM_ putStrLn (tail . drop preCodeLines $ code)
