package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.apache.tools.ant.Location;
import org.junit.Test;

public class AttributeNamespaceDefDiffblueTest {
  /**
   * Test new {@link AttributeNamespaceDef} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link AttributeNamespaceDef}
   */
  @Test
  public void testNewAttributeNamespaceDef() {
    // Arrange and Act
    AttributeNamespaceDef actualAttributeNamespaceDef = new AttributeNamespaceDef();

    // Assert
    assertEquals("", actualAttributeNamespaceDef.getURI());
    assertNull(actualAttributeNamespaceDef.getAntlibClassLoader());
    Location location = actualAttributeNamespaceDef.getLocation();
    assertNull(location.getFileName());
    assertNull(actualAttributeNamespaceDef.getDescription());
    assertNull(actualAttributeNamespaceDef.getTaskName());
    assertNull(actualAttributeNamespaceDef.getTaskType());
    assertNull(actualAttributeNamespaceDef.getProject());
    assertNull(actualAttributeNamespaceDef.getOwningTarget());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}
