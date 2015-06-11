import java.util.concurrent.locks.ReentrantLock;


class BetterSafe implements State {
    private byte[] value;
    private byte maxval;
    private ReentrantLock state_lock;

    BetterSafe(byte[] v) { 
        value = v; maxval = 127; 
        state_lock = new ReentrantLock();
    }

    BetterSafe(byte[] v, byte m) { 
        value = v; maxval = m;
        state_lock = new ReentrantLock();
    }

    public int size() { return value.length; }

    public byte[] current() { return value; }

    public boolean swap(int i, int j) {
        state_lock.lock();
	   if (value[i] <= 0 || value[j] >= maxval) {
            state_lock.unlock();
	       return false;
	   }
	   value[i]--;
	   value[j]++;
       state_lock.unlock();
	   return true;
    }
}
