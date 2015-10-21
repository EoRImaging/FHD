FUNCTION inf_test,test
i=where(Finite(test,/inf),n_inf)
RETURN,n_inf
END