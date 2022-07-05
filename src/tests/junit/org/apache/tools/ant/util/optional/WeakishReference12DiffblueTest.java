package org.apache.tools.ant.util.optional;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class WeakishReference12DiffblueTest {
  /**
  * Method under test: {@link WeakishReference12#WeakishReference12(Object)}
  */
  @Test
  public void testConstructor() {
    // Arrange, Act and Assert
    assertEquals("Reference", (new WeakishReference12("Reference")).get());
  }
}

