package org.apache.tools.ant.taskdefs.optional.depend.constantpool;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class InvokeDynamicCPInfoDiffblueTest {
  /**
   * Test new {@link InvokeDynamicCPInfo} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link InvokeDynamicCPInfo}
   */
  @Test
  public void testNewInvokeDynamicCPInfo() {
    // Arrange and Act
    InvokeDynamicCPInfo actualInvokeDynamicCPInfo = new InvokeDynamicCPInfo();

    // Assert
    assertNull(actualInvokeDynamicCPInfo.getValue());
    assertEquals(1, actualInvokeDynamicCPInfo.getNumEntries());
    assertFalse(actualInvokeDynamicCPInfo.isResolved());
    assertEquals(ConstantPoolEntry.CONSTANT_INVOKEDYNAMIC, actualInvokeDynamicCPInfo.getTag());
  }

  /**
   * Test {@link InvokeDynamicCPInfo#toString()}.
   * <p>
   * Method under test: {@link InvokeDynamicCPInfo#toString()}
   */
  @Test
  public void testToString() {
    // Arrange, Act and Assert
    assertEquals("BootstrapMethodAttrIndex inx = 0NameAndType index = 0", (new InvokeDynamicCPInfo()).toString());
  }
}
