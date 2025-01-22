package org.apache.tools.ant.taskdefs.optional.depend.constantpool;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class MethodTypeCPInfoDiffblueTest {
  /**
   * Test new {@link MethodTypeCPInfo} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link MethodTypeCPInfo}
   */
  @Test
  public void testNewMethodTypeCPInfo() {
    // Arrange and Act
    MethodTypeCPInfo actualMethodTypeCPInfo = new MethodTypeCPInfo();

    // Assert
    assertNull(actualMethodTypeCPInfo.getValue());
    assertEquals(1, actualMethodTypeCPInfo.getNumEntries());
    assertFalse(actualMethodTypeCPInfo.isResolved());
    assertEquals(ConstantPoolEntry.CONSTANT_METHODTYPE, actualMethodTypeCPInfo.getTag());
  }

  /**
   * Test {@link MethodTypeCPInfo#toString()}.
   * <p>
   * Method under test: {@link MethodTypeCPInfo#toString()}
   */
  @Test
  public void testToString() {
    // Arrange, Act and Assert
    assertEquals("MethodDescriptorIndex: 0", (new MethodTypeCPInfo()).toString());
  }
}
