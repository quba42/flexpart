

class BasicTest(object):

    """
    Container for BasicTest properties
    """

    def __init__(self, descr=None, test_type=None, threshold=None):
        
        if descr:
            self._description = descr
        else:
            raise Exception('descr not defined')   

        if test_type:
            self._test_type = test_type
        else:
            raise Exception('test_type not defined')   

        if threshold:
            self._threshold = threshold
            try:
                dummy_var = float(threshold)
                self._threshold = dummy_var 
            except:
                raise Exception('threshold not a valid number')
        else:
            raise Exception('threshold not defined')   


    def get_descr(self):
        return self._description

    def get_test_type(self):
        return self._test_type

    def get_threshold(self):
        return self._threshold



