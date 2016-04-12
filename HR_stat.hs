import Data.List
import Data.Maybe

bar="1908    January 5.0 -1.4\n1908    February    7.3 1.9\n1908    March   6.2 0.3\n1908    April   Missing_1   2.1\n1908    May Missing_2   7.7"
barr::[(Int, Maybe Double)]
barr=[(1,Just 1),(13,Just 2), (25,Just 3), (37,Just 4)]
findBetas :: [(Int, Maybe Double)] -> [(Double,Double)]
findBetas l1 = map (findBetas' . filt) [0..11]
  where filt n = filter (\x -> fst x `rem` 12 == n) l1
  
findBetas' :: [(Int, Maybe Double)] -> (Double, Double) -- (beta0, beta1)
findBetas' l1 = (beta0, beta1)
  where xy1 = sum . map (\(x,y) -> fromIntegral x * fromJust y) $ l1
        x2_1 = sum . map (\(x,_) -> x^2) $ l1
--        beta1 = xy1 / fromIntegral x2_1
        avgx1 = (/ fromIntegral (length l1)) . fromIntegral . sum . map fst $ l1
        avgy1 = (/ fromIntegral (length l1)) . sum . map (fromJust . snd) $ l1
        dividend = sum . map (\(x,y) -> (fromIntegral x - avgx1)* (fromJust y - avgy1)) $ l1
        divisor = sum . map (\(x,y) -> (fromIntegral x - avgx1)^2) $ l1
        beta1 = dividend / divisor
        beta0 = avgy1 - beta1 * avgx1
calcy1 beta x = fst (beta) + (fromIntegral x) * snd (beta)
go :: [(Int, (Maybe Double,Maybe Double))] -> [Double]
go l = reverse $ foldl' solve [] l
  where l1 = filter ((/= Nothing).snd) . map (\(x,y)->(x,fst y)) $ l
        l2 = filter ((/= Nothing).snd) . map (\(x,y)->(x,snd y)) $ l
        betas1 = findBetas l1
        betas2 = findBetas l2
        calcy1 x = fst (betas1 !! (x `rem` 12)) + (fromIntegral x) * snd (betas1 !! (x `rem` 12))
        calcy2 x = fst (betas2 !! (x `rem` 12)) + (fromIntegral x) * snd (betas2 !! (x `rem` 12))
        solve :: [Double] -> (Int, (Maybe Double, Maybe Double)) -> [Double]
        solve acc (x,(y1,y2)) =
          case (y1,y2) of
            (Nothing, Nothing) -> calcy2 x : calcy1 x : acc
            (Nothing, _)       -> calcy1 x : acc
            (_, Nothing)       -> calcy2 x : acc
            otherwise          -> acc

conv :: (Int, String,String) -> (Int, (Maybe Double, Maybe Double))
conv (idx,x,y) = let x' = if isMissing x then Nothing else Just (read x)
                     y' = if isMissing y then Nothing else Just (read y)
                 in (idx, (x', y'))
  where isMissing  = isPrefixOf "Missing_"

merge :: [String] -> (Int, String, String)
merge [a,b,c,d] = (idx,c,d)
  where a' = read a :: Int
        b' = fromJust $ elemIndex b
          ["January", "February", "March", "April", "May", "June",
           "July", "August", "September", "October", "November", "December"]
        idx = a' * 12 + b'
  
foo :: IO ()
foo = getLine >> getLine >>
      (mapM_ (putStrLn . show) . go . 
       map (conv. merge . words) . lines =<< getContents)

main = foo
