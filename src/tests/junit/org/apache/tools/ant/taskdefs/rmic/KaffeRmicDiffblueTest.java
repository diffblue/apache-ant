package org.apache.tools.ant.taskdefs.rmic;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class KaffeRmicDiffblueTest {
  /**
   * Test {@link KaffeRmic#areIiopAndIdlSupported()}.
   * <p>
   * Method under test: {@link KaffeRmic#areIiopAndIdlSupported()}
   */
  @Test
  public void testAreIiopAndIdlSupported() {
    // Arrange, Act and Assert
    assertTrue((new KaffeRmic()).areIiopAndIdlSupported());
  }

  /**
   * Test {@link KaffeRmic#isAvailable()}.
   * <p>
   * Method under test: {@link KaffeRmic#isAvailable()}
   */
  @Test
  public void testIsAvailable() {
    // Arrange, Act and Assert
    assertFalse(KaffeRmic.isAvailable());
  }

  /**
   * Test new {@link KaffeRmic} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link KaffeRmic}
   */
  @Test
  public void testNewKaffeRmic() {
    // Arrange and Act
    KaffeRmic actualKaffeRmic = new KaffeRmic();

    // Assert
    assertNull(actualKaffeRmic.getRmic());
    assertNull(actualKaffeRmic.getMapper());
  }
}
