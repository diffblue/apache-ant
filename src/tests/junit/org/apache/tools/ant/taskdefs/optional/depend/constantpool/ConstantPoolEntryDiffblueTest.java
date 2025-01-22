package org.apache.tools.ant.taskdefs.optional.depend.constantpool;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class ConstantPoolEntryDiffblueTest {
  /**
   * Test {@link ConstantPoolEntry#isResolved()}.
   * <p>
   * Method under test: {@link ConstantPoolEntry#isResolved()}
   */
  @Test
  public void testIsResolved() {
    // Arrange, Act and Assert
    assertFalse((new ClassCPInfo()).isResolved());
  }

  /**
   * Test {@link ConstantPoolEntry#resolve(ConstantPool)}.
   * <p>
   * Method under test: {@link ConstantPoolEntry#resolve(ConstantPool)}
   */
  @Test
  public void testResolve() {
    // Arrange
    DoubleCPInfo doubleCPInfo = new DoubleCPInfo();

    // Act
    doubleCPInfo.resolve(new ConstantPool());

    // Assert
    assertTrue(doubleCPInfo.isResolved());
  }

  /**
   * Test {@link ConstantPoolEntry#getTag()}.
   * <p>
   * Method under test: {@link ConstantPoolEntry#getTag()}
   */
  @Test
  public void testGetTag() {
    // Arrange, Act and Assert
    assertEquals(7, (new ClassCPInfo()).getTag());
  }

  /**
   * Test {@link ConstantPoolEntry#getNumEntries()}.
   * <p>
   * Method under test: {@link ConstantPoolEntry#getNumEntries()}
   */
  @Test
  public void testGetNumEntries() {
    // Arrange, Act and Assert
    assertEquals(1, (new ClassCPInfo()).getNumEntries());
  }
}
