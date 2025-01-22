package org.apache.tools.ant.taskdefs.optional.depend.constantpool;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import org.apache.tools.ant.taskdefs.optional.depend.constantpool.MethodHandleCPInfo.ReferenceKind;
import org.junit.Test;

public class MethodHandleCPInfoDiffblueTest {
  /**
   * Test new {@link MethodHandleCPInfo} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link MethodHandleCPInfo}
   */
  @Test
  public void testNewMethodHandleCPInfo() {
    // Arrange and Act
    MethodHandleCPInfo actualMethodHandleCPInfo = new MethodHandleCPInfo();

    // Assert
    assertEquals(1, actualMethodHandleCPInfo.getNumEntries());
    assertFalse(actualMethodHandleCPInfo.isResolved());
    assertEquals(ConstantPoolEntry.CONSTANT_METHODHANDLE, actualMethodHandleCPInfo.getTag());
  }

  /**
   * Test ReferenceKind {@link ReferenceKind#value()}.
   * <p>
   * Method under test: {@link ReferenceKind#value()}
   */
  @Test
  public void testReferenceKindValue() {
    // Arrange, Act and Assert
    assertEquals(1, ReferenceKind.REF_getField.value());
  }

  /**
   * Test {@link MethodHandleCPInfo#toString()}.
   * <p>
   * Method under test: {@link MethodHandleCPInfo#toString()}
   */
  @Test
  public void testToString() {
    // Arrange, Act and Assert
    assertEquals("MethodHandle : Reference kind = nullReference index = 0", (new MethodHandleCPInfo()).toString());
  }
}
