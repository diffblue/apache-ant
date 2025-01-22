package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class KeySubstDiffblueTest {
  /**
   * Test new {@link KeySubst} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link KeySubst}
   */
  @Test
  public void testNewKeySubst() {
    // Arrange and Act
    KeySubst actualKeySubst = new KeySubst();

    // Assert
    Location location = actualKeySubst.getLocation();
    assertNull(location.getFileName());
    assertNull(actualKeySubst.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualKeySubst.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualKeySubst.getTaskName());
    assertNull(actualKeySubst.getTaskType());
    assertNull(actualKeySubst.getProject());
    assertNull(actualKeySubst.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualKeySubst, runtimeConfigurableWrapper.getProxy());
  }
}
