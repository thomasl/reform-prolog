#define InitMarkBuffer(W)						   \
{									   \
  AcquireLock(W->gc_info.load->lock);					   \
  W->gc_info.load->start = W->gc_info.load->stop = 0;			   \
  ReleaseLock(W->gc_info.load->lock);					   \
}

#define BufferFull(W)                                                      \
((((W)->gc_info.load->stop+1)&LOADBUFFERSIZE) == (W)->gc_info.load->start)

#define AddMarkBuffer(W,SIZE,ARG)					   \
{									   \
  int amb_index;							   \
									   \
  AcquireLock((W)->gc_info.load->lock);					   \
									   \
  amb_index = ((W)->gc_info.load->stop+1)&LOADBUFFERSIZE;		   \
									   \
  /* Check if buffer is full */						   \
  if (amb_index !=  (W)->gc_info.load->start)				   \
    {									   \
      /* Add new entry */						   \
      (W)->gc_info.load->stop = amb_index;				   \
      (W)->gc_info.load->buf[amb_index].size = SIZE;			   \
      (W)->gc_info.load->buf[amb_index].arg = ARG;			   \
    }									   \
									   \
  ReleaseLock((W)->gc_info.load->lock);					   \
}

#define RemoveMarkBufferHead(W,SIZE,ARG)				   \
{									   \
  int rmb_index;							   \
									   \
  AcquireLock((W)->gc_info.load->lock);					   \
									   \
  if ((W)->gc_info.load->stop != (W)->gc_info.load->start)		   \
    {									   \
      rmb_index = ((W)->gc_info.load->start+1)&LOADBUFFERSIZE;		   \
      SIZE = (W)->gc_info.load->buf[rmb_index].size;			   \
      ARG =  (W)->gc_info.load->buf[rmb_index].arg;			   \
      (W)->gc_info.load->start = rmb_index;				   \
    }									   \
  else									   \
    {									   \
      SIZE = 0;								   \
    }									   \
									   \
  ReleaseLock((W)->gc_info.load->lock);					   \
}

#define RemoveMarkBufferTail(W,SIZE,ARG)				   \
{									   \
  int rmb_index;							   \
									   \
  AcquireLock((W)->gc_info.load->lock);					   \
									   \
  if ((W)->gc_info.load->stop != (W)->gc_info.load->start)		   \
    {									   \
      rmb_index = (W)->gc_info.load->stop;				   \
      if ((SIZE == (W)->gc_info.load->buf[rmb_index].size) &&		   \
	  (ARG ==  (W)->gc_info.load->buf[rmb_index].arg))		   \
	{								   \
	  (W)->gc_info.load->stop = (rmb_index-1)&LOADBUFFERSIZE;	   \
	}								   \
    }									   \
  else									   \
    {									   \
      SIZE = 0;								   \
    }									   \
									   \
  ReleaseLock((W)->gc_info.load->lock);					   \
}

