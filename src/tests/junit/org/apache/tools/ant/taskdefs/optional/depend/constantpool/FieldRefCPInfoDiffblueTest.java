package org.apache.tools.ant.taskdefs.optional.depend.constantpool;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class FieldRefCPInfoDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link FieldRefCPInfo}
   *   <li>{@link FieldRefCPInfo#getFieldClassName()}
   *   <li>{@link FieldRefCPInfo#getFieldName()}
   *   <li>{@link FieldRefCPInfo#getFieldType()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    FieldRefCPInfo actualFieldRefCPInfo = new FieldRefCPInfo();
    String actualFieldClassName = actualFieldRefCPInfo.getFieldClassName();
    String actualFieldName = actualFieldRefCPInfo.getFieldName();

    // Assert
    assertNull(actualFieldClassName);
    assertNull(actualFieldName);
    assertNull(actualFieldRefCPInfo.getFieldType());
    assertEquals(1, actualFieldRefCPInfo.getNumEntries());
    assertFalse(actualFieldRefCPInfo.isResolved());
    assertEquals(ConstantPoolEntry.CONSTANT_FIELDREF, actualFieldRefCPInfo.getTag());
  }

  /**
   * Test {@link FieldRefCPInfo#toString()}.
   * <p>
   * Method under test: {@link FieldRefCPInfo#toString()}
   */
  @Test
  public void testToString() {
    // Arrange, Act and Assert
    assertEquals("Field : Class index = 0, name and type index = 0", (new FieldRefCPInfo()).toString());
  }
}
