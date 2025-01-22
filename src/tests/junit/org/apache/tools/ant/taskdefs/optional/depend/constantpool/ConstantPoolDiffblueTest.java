package org.apache.tools.ant.taskdefs.optional.depend.constantpool;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import org.junit.Test;

public class ConstantPoolDiffblueTest {
  /**
   * Test new {@link ConstantPool} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ConstantPool}
   */
  @Test
  public void testNewConstantPool() {
    // Arrange, Act and Assert
    assertEquals(1, (new ConstantPool()).size());
  }

  /**
   * Test {@link ConstantPool#size()}.
   * <p>
   * Method under test: {@link ConstantPool#size()}
   */
  @Test
  public void testSize() {
    // Arrange, Act and Assert
    assertEquals(1, (new ConstantPool()).size());
  }

  /**
   * Test {@link ConstantPool#addEntry(ConstantPoolEntry)}.
   * <ul>
   *   <li>Given {@link ConstantPool} (default constructor).</li>
   *   <li>When {@link ClassCPInfo} (default constructor).</li>
   *   <li>Then {@link ConstantPool} (default constructor) size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link ConstantPool#addEntry(ConstantPoolEntry)}
   */
  @Test
  public void testAddEntry_givenConstantPool_whenClassCPInfo_thenConstantPoolSizeIsTwo() {
    // Arrange
    ConstantPool constantPool = new ConstantPool();

    // Act and Assert
    assertEquals(1, constantPool.addEntry(new ClassCPInfo()));
    assertEquals(2, constantPool.size());
  }

  /**
   * Test {@link ConstantPool#addEntry(ConstantPoolEntry)}.
   * <ul>
   *   <li>Given {@link ConstantPool} (default constructor).</li>
   *   <li>When {@link DoubleCPInfo} (default constructor).</li>
   *   <li>Then {@link ConstantPool} (default constructor) size is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ConstantPool#addEntry(ConstantPoolEntry)}
   */
  @Test
  public void testAddEntry_givenConstantPool_whenDoubleCPInfo_thenConstantPoolSizeIsThree() {
    // Arrange
    ConstantPool constantPool = new ConstantPool();

    // Act and Assert
    assertEquals(1, constantPool.addEntry(new DoubleCPInfo()));
    assertEquals(3, constantPool.size());
  }

  /**
   * Test {@link ConstantPool#addEntry(ConstantPoolEntry)}.
   * <ul>
   *   <li>Given {@link ConstantPool} (default constructor).</li>
   *   <li>When {@link Utf8CPInfo} (default constructor).</li>
   *   <li>Then {@link ConstantPool} (default constructor) size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link ConstantPool#addEntry(ConstantPoolEntry)}
   */
  @Test
  public void testAddEntry_givenConstantPool_whenUtf8CPInfo_thenConstantPoolSizeIsTwo() {
    // Arrange
    ConstantPool constantPool = new ConstantPool();

    // Act and Assert
    assertEquals(1, constantPool.addEntry(new Utf8CPInfo()));
    assertEquals(2, constantPool.size());
  }

  /**
   * Test {@link ConstantPool#getEntry(int)}.
   * <ul>
   *   <li>Given {@link ConstantPool} (default constructor) addEntry {@link ClassCPInfo} (default constructor).</li>
   *   <li>Then return {@link ClassCPInfo} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ConstantPool#getEntry(int)}
   */
  @Test
  public void testGetEntry_givenConstantPoolAddEntryClassCPInfo_thenReturnClassCPInfo() {
    // Arrange
    ConstantPool constantPool = new ConstantPool();
    ClassCPInfo entry = new ClassCPInfo();
    constantPool.addEntry(entry);

    // Act and Assert
    assertSame(entry, constantPool.getEntry(1));
  }

  /**
   * Test {@link ConstantPool#getUTF8Entry(String)}.
   * <p>
   * Method under test: {@link ConstantPool#getUTF8Entry(String)}
   */
  @Test
  public void testGetUTF8Entry() {
    // Arrange, Act and Assert
    assertEquals(-1, (new ConstantPool()).getUTF8Entry("42"));
  }

  /**
   * Test {@link ConstantPool#getClassEntry(String)}.
   * <ul>
   *   <li>Given {@link ConstantPool} (default constructor).</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ConstantPool#getClassEntry(String)}
   */
  @Test
  public void testGetClassEntry_givenConstantPool_thenReturnMinusOne() {
    // Arrange, Act and Assert
    assertEquals(-1, (new ConstantPool()).getClassEntry("Class Name"));
  }

  /**
   * Test {@link ConstantPool#getConstantEntry(Object)}.
   * <ul>
   *   <li>Given {@link ConstantPool} (default constructor).</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ConstantPool#getConstantEntry(Object)}
   */
  @Test
  public void testGetConstantEntry_givenConstantPool_thenReturnMinusOne() {
    // Arrange, Act and Assert
    assertEquals(-1, (new ConstantPool()).getConstantEntry("Constant Value"));
  }

  /**
   * Test {@link ConstantPool#getConstantEntry(Object)}.
   * <ul>
   *   <li>Given {@link DoubleCPInfo} (default constructor) Value is {@code Constant Value}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ConstantPool#getConstantEntry(Object)}
   */
  @Test
  public void testGetConstantEntry_givenDoubleCPInfoValueIsConstantValue_thenReturnOne() {
    // Arrange
    DoubleCPInfo entry = new DoubleCPInfo();
    entry.setValue("Constant Value");

    ConstantPool constantPool = new ConstantPool();
    constantPool.addEntry(entry);

    // Act and Assert
    assertEquals(1, constantPool.getConstantEntry("Constant Value"));
  }

  /**
   * Test {@link ConstantPool#getConstantEntry(Object)}.
   * <ul>
   *   <li>Given {@link DoubleCPInfo} (default constructor) Value is {@code New Value}.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ConstantPool#getConstantEntry(Object)}
   */
  @Test
  public void testGetConstantEntry_givenDoubleCPInfoValueIsNewValue_thenReturnMinusOne() {
    // Arrange
    DoubleCPInfo entry = new DoubleCPInfo();
    entry.setValue("New Value");

    ConstantPool constantPool = new ConstantPool();
    constantPool.addEntry(entry);

    // Act and Assert
    assertEquals(-1, constantPool.getConstantEntry("Constant Value"));
  }

  /**
   * Test {@link ConstantPool#getMethodRefEntry(String, String, String)}.
   * <p>
   * Method under test: {@link ConstantPool#getMethodRefEntry(String, String, String)}
   */
  @Test
  public void testGetMethodRefEntry() {
    // Arrange, Act and Assert
    assertEquals(-1, (new ConstantPool()).getMethodRefEntry("Method Class Name", "Method Name", "Method Type"));
  }

  /**
   * Test {@link ConstantPool#getInterfaceMethodRefEntry(String, String, String)}.
   * <ul>
   *   <li>Given {@link ConstantPool} (default constructor).</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ConstantPool#getInterfaceMethodRefEntry(String, String, String)}
   */
  @Test
  public void testGetInterfaceMethodRefEntry_givenConstantPool_thenReturnMinusOne() {
    // Arrange, Act and Assert
    assertEquals(-1, (new ConstantPool()).getInterfaceMethodRefEntry("Interface Method Class Name",
        "Interface Method Name", "Interface Method Type"));
  }

  /**
   * Test {@link ConstantPool#getFieldRefEntry(String, String, String)}.
   * <ul>
   *   <li>Given {@link ConstantPool} (default constructor).</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ConstantPool#getFieldRefEntry(String, String, String)}
   */
  @Test
  public void testGetFieldRefEntry_givenConstantPool_thenReturnMinusOne() {
    // Arrange, Act and Assert
    assertEquals(-1, (new ConstantPool()).getFieldRefEntry("Field Class Name", "Field Name", "Field Type"));
  }

  /**
   * Test {@link ConstantPool#getNameAndTypeEntry(String, String)}.
   * <ul>
   *   <li>Given {@link ConstantPool} (default constructor).</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ConstantPool#getNameAndTypeEntry(String, String)}
   */
  @Test
  public void testGetNameAndTypeEntry_givenConstantPool_thenReturnMinusOne() {
    // Arrange, Act and Assert
    assertEquals(-1, (new ConstantPool()).getNameAndTypeEntry("Name", "Type"));
  }

  /**
   * Test {@link ConstantPool#toString()}.
   * <p>
   * Method under test: {@link ConstantPool#toString()}
   */
  @Test
  public void testToString() {
    // Arrange, Act and Assert
    assertEquals("\n[0] = null\n", (new ConstantPool()).toString());
  }
}
