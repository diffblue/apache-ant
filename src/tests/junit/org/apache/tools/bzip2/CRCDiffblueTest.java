package org.apache.tools.bzip2;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class CRCDiffblueTest {
  /**
   * Test new {@link CRC} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CRC}
   */
  @Test
  public void testNewCrc() {
    // Arrange and Act
    CRC actualCrc = new CRC();

    // Assert
    assertEquals(-1, actualCrc.getGlobalCRC());
    assertEquals(0, actualCrc.getFinalCRC());
  }

  /**
   * Test {@link CRC#getFinalCRC()}.
   * <p>
   * Method under test: {@link CRC#getFinalCRC()}
   */
  @Test
  public void testGetFinalCRC() {
    // Arrange, Act and Assert
    assertEquals(0, (new CRC()).getFinalCRC());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link CRC#setGlobalCRC(int)}
   *   <li>{@link CRC#initialiseCRC()}
   *   <li>{@link CRC#getGlobalCRC()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    CRC crc = new CRC();

    // Act
    crc.setGlobalCRC(1);
    crc.initialiseCRC();

    // Assert
    assertEquals(-1, crc.getGlobalCRC());
  }

  /**
   * Test {@link CRC#updateCRC(int, int)} with {@code inCh}, {@code repeat}.
   * <ul>
   *   <li>When minus two.</li>
   *   <li>Then {@link CRC} (default constructor) GlobalCRC is {@code -79764809}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CRC#updateCRC(int, int)}
   */
  @Test
  public void testUpdateCRCWithInChRepeat_whenMinusTwo_thenCrcGlobalCRCIs79764809() {
    // Arrange
    CRC crc = new CRC();

    // Act
    crc.updateCRC(-2, 1);

    // Assert
    assertEquals(-79764809, crc.getGlobalCRC());
    assertEquals(79764808, crc.getFinalCRC());
  }

  /**
   * Test {@link CRC#updateCRC(int, int)} with {@code inCh}, {@code repeat}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then {@link CRC} (default constructor) FinalCRC is {@code -1254728196}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CRC#updateCRC(int, int)}
   */
  @Test
  public void testUpdateCRCWithInChRepeat_whenOne_thenCrcFinalCRCIs1254728196() {
    // Arrange
    CRC crc = new CRC();

    // Act
    crc.updateCRC(1, 1);

    // Assert
    assertEquals(-1254728196, crc.getFinalCRC());
    assertEquals(1254728195, crc.getGlobalCRC());
  }

  /**
   * Test {@link CRC#updateCRC(int)} with {@code inCh}.
   * <ul>
   *   <li>When minus two.</li>
   *   <li>Then {@link CRC} (default constructor) GlobalCRC is {@code -79764809}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CRC#updateCRC(int)}
   */
  @Test
  public void testUpdateCRCWithInCh_whenMinusTwo_thenCrcGlobalCRCIs79764809() {
    // Arrange
    CRC crc = new CRC();

    // Act
    crc.updateCRC(-2);

    // Assert
    assertEquals(-79764809, crc.getGlobalCRC());
    assertEquals(79764808, crc.getFinalCRC());
  }

  /**
   * Test {@link CRC#updateCRC(int)} with {@code inCh}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then {@link CRC} (default constructor) FinalCRC is {@code -1254728196}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CRC#updateCRC(int)}
   */
  @Test
  public void testUpdateCRCWithInCh_whenOne_thenCrcFinalCRCIs1254728196() {
    // Arrange
    CRC crc = new CRC();

    // Act
    crc.updateCRC(1);

    // Assert
    assertEquals(-1254728196, crc.getFinalCRC());
    assertEquals(1254728195, crc.getGlobalCRC());
  }
}
