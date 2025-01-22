package org.apache.tools.ant.taskdefs.optional.depend.constantpool;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.IOException;
import org.junit.Test;

public class DoubleCPInfoDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link DoubleCPInfo}
   *   <li>{@link DoubleCPInfo#toString()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    DoubleCPInfo actualDoubleCPInfo = new DoubleCPInfo();

    // Assert
    assertEquals("Double Constant Pool Entry: null", actualDoubleCPInfo.toString());
    assertNull(actualDoubleCPInfo.getValue());
    assertEquals(2, actualDoubleCPInfo.getNumEntries());
    assertEquals(6, actualDoubleCPInfo.getTag());
    assertFalse(actualDoubleCPInfo.isResolved());
  }

  /**
   * Test {@link DoubleCPInfo#read(DataInputStream)}.
   * <ul>
   *   <li>Then {@link DoubleCPInfo} (default constructor) Value doubleValue is {@code 6358369.021011673}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DoubleCPInfo#read(DataInputStream)}
   */
  @Test
  public void testRead_thenDoubleCPInfoValueDoubleValueIs6358369021011673() throws IOException {
    // Arrange
    DoubleCPInfo doubleCPInfo = new DoubleCPInfo();

    // Act
    doubleCPInfo.read(new DataInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"))));

    // Assert
    assertEquals(6358369.021011673d, ((Double) doubleCPInfo.getValue()).doubleValue(), 0.0);
  }
}
