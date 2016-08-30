
readDWIBvec :: DWI -> Action [Dir]
readDWIBvec dwi = readbvec (replaceExtension' f "bvec")
  where f = filepath dwi

data Dir = Dir { v1::Double,
                v2::Double,
                v3::Double }

instance Show Dir where
  show (Dir v1 v2 v3) = printf "%f %f %f" v1 v2 v3
