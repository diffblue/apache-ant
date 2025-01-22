package org.apache.tools.ant.taskdefs.email;

import static org.junit.Assert.assertFalse;
import org.junit.Test;

public class PlainMailerDiffblueTest {
  /**
   * Test new {@link PlainMailer} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link PlainMailer}
   */
  @Test
  public void testNewPlainMailer() {
    // Arrange and Act
    PlainMailer actualPlainMailer = new PlainMailer();

    // Assert
    assertFalse(actualPlainMailer.isPortExplicitlySpecified());
    assertFalse(actualPlainMailer.isStartTLSEnabled());
  }
}
