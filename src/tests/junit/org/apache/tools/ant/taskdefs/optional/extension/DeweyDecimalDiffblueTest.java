package org.apache.tools.ant.taskdefs.optional.extension;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class DeweyDecimalDiffblueTest {
  /**
   * Test {@link DeweyDecimal#DeweyDecimal(int[])}.
   * <p>
   * Method under test: {@link DeweyDecimal#DeweyDecimal(int[])}
   */
  @Test
  public void testNewDeweyDecimal() {
    // Arrange, Act and Assert
    assertEquals(4, (new DeweyDecimal(new int[]{1, -1, 1, -1})).getSize());
  }

  /**
   * Test {@link DeweyDecimal#DeweyDecimal(String)}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then return Size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link DeweyDecimal#DeweyDecimal(String)}
   */
  @Test
  public void testNewDeweyDecimal_when42_thenReturnSizeIsOne() throws NumberFormatException {
    // Arrange, Act and Assert
    assertEquals(1, (new DeweyDecimal("42")).getSize());
  }
}
