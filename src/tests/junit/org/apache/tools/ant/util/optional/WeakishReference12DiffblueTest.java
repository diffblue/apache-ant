package org.apache.tools.ant.util.optional;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class WeakishReference12DiffblueTest {
  /**
   * Test {@link WeakishReference12#WeakishReference12(Object)}.
   * <p>
   * Method under test: {@link WeakishReference12#WeakishReference12(Object)}
   */
  @Test
  public void testNewWeakishReference12() {
    // Arrange, Act and Assert
    assertEquals("Reference", (new WeakishReference12("Reference")).get());
  }
}
