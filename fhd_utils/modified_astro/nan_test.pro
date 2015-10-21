FUNCTION nan_test,test
i=where(Finite(test,/nan),n_nan)
RETURN,n_nan
END