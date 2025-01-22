package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class SendEmailDiffblueTest {
  /**
   * Test new {@link SendEmail} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link SendEmail}
   */
  @Test
  public void testNewSendEmail() {
    // Arrange and Act
    SendEmail actualSendEmail = new SendEmail();

    // Assert
    Location location = actualSendEmail.getLocation();
    assertNull(location.getFileName());
    assertNull(actualSendEmail.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualSendEmail.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualSendEmail.getTaskName());
    assertNull(actualSendEmail.getTaskType());
    assertNull(actualSendEmail.getCharset());
    assertNull(actualSendEmail.getProject());
    assertNull(actualSendEmail.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualSendEmail.getIncludeFileNames());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualSendEmail, runtimeConfigurableWrapper.getProxy());
  }
}
