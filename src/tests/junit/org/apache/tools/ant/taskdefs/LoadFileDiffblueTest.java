package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class LoadFileDiffblueTest {
  /**
   * Test new {@link LoadFile} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link LoadFile}
   */
  @Test
  public void testNewLoadFile() {
    // Arrange and Act
    LoadFile actualLoadFile = new LoadFile();

    // Assert
    Location location = actualLoadFile.getLocation();
    assertNull(location.getFileName());
    assertNull(actualLoadFile.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualLoadFile.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualLoadFile.getTaskName());
    assertNull(actualLoadFile.getTaskType());
    assertNull(actualLoadFile.getProject());
    assertNull(actualLoadFile.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualLoadFile, runtimeConfigurableWrapper.getProxy());
  }
}
