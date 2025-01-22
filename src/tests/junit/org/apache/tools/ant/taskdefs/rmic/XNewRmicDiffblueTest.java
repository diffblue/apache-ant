package org.apache.tools.ant.taskdefs.rmic;

import static org.junit.Assert.assertNull;
import org.junit.Test;

public class XNewRmicDiffblueTest {
  /**
   * Test new {@link XNewRmic} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link XNewRmic}
   */
  @Test
  public void testNewXNewRmic() {
    // Arrange and Act
    XNewRmic actualXNewRmic = new XNewRmic();

    // Assert
    assertNull(actualXNewRmic.getRmic());
    assertNull(actualXNewRmic.getMapper());
  }
}
