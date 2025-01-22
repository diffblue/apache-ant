package org.apache.tools.ant.taskdefs.optional.net;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class MimeMailDiffblueTest {
  /**
   * Test new {@link MimeMail} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link MimeMail}
   */
  @Test
  public void testNewMimeMail() {
    // Arrange and Act
    MimeMail actualMimeMail = new MimeMail();

    // Assert
    Location location = actualMimeMail.getLocation();
    assertNull(location.getFileName());
    assertNull(actualMimeMail.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualMimeMail.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualMimeMail.getTaskName());
    assertNull(actualMimeMail.getTaskType());
    assertNull(actualMimeMail.getCharset());
    assertNull(actualMimeMail.getProject());
    assertNull(actualMimeMail.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualMimeMail.getIncludeFileNames());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualMimeMail, runtimeConfigurableWrapper.getProxy());
  }
}
