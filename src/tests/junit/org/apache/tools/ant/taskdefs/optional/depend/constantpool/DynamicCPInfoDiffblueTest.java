package org.apache.tools.ant.taskdefs.optional.depend.constantpool;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class DynamicCPInfoDiffblueTest {
  /**
   * Test new {@link DynamicCPInfo} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link DynamicCPInfo}
   */
  @Test
  public void testNewDynamicCPInfo() {
    // Arrange and Act
    DynamicCPInfo actualDynamicCPInfo = new DynamicCPInfo();

    // Assert
    assertNull(actualDynamicCPInfo.getValue());
    assertEquals(1, actualDynamicCPInfo.getNumEntries());
    assertFalse(actualDynamicCPInfo.isResolved());
    assertEquals(ConstantPoolEntry.CONSTANT_DYNAMIC, actualDynamicCPInfo.getTag());
  }

  /**
   * Test {@link DynamicCPInfo#toString()}.
   * <p>
   * Method under test: {@link DynamicCPInfo#toString()}
   */
  @Test
  public void testToString() {
    // Arrange, Act and Assert
    assertEquals("BootstrapMethodAttrIndex inx = 0NameAndType index = 0", (new DynamicCPInfo()).toString());
  }
}
