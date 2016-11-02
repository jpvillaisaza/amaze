module Data.Stack
  ( Stack
  , pop
  , push
  )
  where


-- |
--
--

type Stack a =
  [a]


-- |
--
--

push
  :: a
  -> Stack a
  -> Stack a

push =
  (:)


-- |
--
--

pop
  :: Stack a
  -> Maybe (a, Stack a)

pop xxs =
  case xxs of
    [] ->
      Nothing

    x:xs ->
      Just (x, xs)
