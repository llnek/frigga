package czlab.frigga.test;

import java.util.concurrent.atomic.AtomicInteger;
import junit.framework.JUnit4TestAdapter;
import static org.junit.Assert.assertEquals;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class JUnit {

  public static junit.framework.Test suite() {
    return
    new JUnit4TestAdapter(JUnit.class);
  }

  @BeforeClass
  public static void iniz() throws Exception {
  }

  @AfterClass
  public static void finz() {
  }

  @Before
  public void open() throws Exception {
  }

  @After
  public void close() throws Exception {
  }

  @Test
  public void testDummy() throws Exception {
    assertEquals(1, 1);
  }

}

