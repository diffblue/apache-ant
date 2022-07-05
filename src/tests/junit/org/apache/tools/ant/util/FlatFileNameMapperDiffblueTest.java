package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class FlatFileNameMapperDiffblueTest {
  /**
  * Method under test: {@link FlatFileNameMapper#mapFileName(String)}
  */
  @Test
  public void testMapFileName() {
    // Arrange and Act
    String[] actualMapFileNameResult = (new FlatFileNameMapper()).mapFileName("foo.txt");

    // Assert
    assertEquals(1, actualMapFileNameResult.length);
    assertEquals("foo.txt", actualMapFileNameResult[0]);
  }

  /**
   * Method under test: {@link FlatFileNameMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName2() {
    // Arrange, Act and Assert
    assertNull((new FlatFileNameMapper()).mapFileName(null));
  }
}

